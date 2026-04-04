const std = @import("std");
const meta = @import("meta.zig");

const assert = std.debug.assert;

pub const NodeIndex = u31; // u31 to avoid accidental conversion of TokenIndex to this type

pub fn GenericTree(Node: type) type {
    assert(meta.containsField(Node, "first_child"));
    assert(meta.containsField(Node, "next_sibling"));
    return struct {
        nodes: std.ArrayList(Node),
        gpa: std.mem.Allocator,

        const Tree = @This();

        pub fn init(gpa: std.mem.Allocator, initial_capacity: usize) !Tree {
            var t = Tree{
                .nodes = try .initCapacity(gpa, initial_capacity + 1),
                .gpa = gpa,
            };

            t.nodes.appendAssumeCapacity(.{}); // add a single node which represents "invalid" at pos 0.

            return t;
        }

        pub fn deinit(t: *Tree) void {
            t.nodes.deinit(t.gpa);
        }

        pub fn nodeCount(t: *const Tree) usize {
            return t.nodes.items.len;
        }

        pub fn takeSnapshot(t: *const Tree) ![]Node {
            return t.gpa.dupe(Node, t.nodes.items);
        }

        pub fn freeSnapshot(t: *const Tree, snapshot: []Node) void {
            t.gpa.free(snapshot);
        }

        pub fn equals(t: *const Tree, snapshot: []Node) bool {
            if (t.nodes.items.len != snapshot.len)
                return false;

            for (t.nodes.items, snapshot) |a, b|
                if (!std.meta.eql(a, b))
                    return false;

            return true;
        }

        pub fn append(t: *Tree, node: Node) !NodeIndex {
            const res: NodeIndex = @intCast(t.nodes.items.len);
            try t.nodes.append(t.gpa, node);
            return res;
        }

        pub fn add(t: *Tree) !struct { ptr: *Node, idx: NodeIndex } {
            const idx: NodeIndex = @intCast(t.nodes.items.len);
            return .{
                .ptr = try t.nodes.addOne(t.gpa),
                .idx = idx,
            };
        }

        pub fn reserve(t: *Tree) !NodeIndex {
            return try t.append(.{});
        }

        pub fn get(t: *const Tree, index: NodeIndex) *Node {
            const node = &t.nodes.items[index];
            // std.debug.print("ast.get({}).next_sibling == {}\n", .{ index, node.next_sibling });
            return node;
        }

        pub fn countChildrenOf(t: *const Tree, node_idx: NodeIndex) usize {
            var idx = t.get(node_idx).first_child;
            var i: usize = 0;
            while (idx != 0) {
                idx = t.get(idx).next_sibling;
                i += 1;
            }
            return i;
        }

        pub fn startList(t: *Tree) LinkedList {
            return LinkedList{ .tree = t };
        }

        pub fn getList(t: *const Tree, index: NodeIndex) LinkedList {
            return .{
                .tree = @constCast(t),
                .index = if(index==0) null else index,
                .start_index = index,
            };
        }

        pub const LinkedList = struct {
            tree: *Tree,
            index: ?NodeIndex = null,
            start_index: NodeIndex = 0, // will be set when first element is added.
            // count: u32 = 0, // only for writitng! will always be 0 when reading!

            pub fn appendExisting(ll: *LinkedList, node_idx: NodeIndex) !void {
                // std.debug.print("append START:{any}\n", .{ll.ed.data.items});
                assert(node_idx != 0);
                assert(ll.tree.get(node_idx).next_sibling == 0);

                if (ll.index) |prev_elem_i| {
                    const prev_elem = ll.tree.get(prev_elem_i);
                    prev_elem.next_sibling = node_idx;
                } else {
                    ll.start_index = node_idx;
                }
                ll.index = node_idx;
                // ll.count += 1;

                // std.debug.print("append END:  {any}\n", .{ll.ed.data.items});
            }

            pub fn append(ll: *LinkedList, node_data: Node) !void {
                // std.debug.print("append START:{any}\n", .{ll.ed.data.items});
                assert(node_data.next_sibling == 0);

                const node_idx = try ll.tree.reserve();
                const node = ll.tree.get(node_idx);
                node.* = node_data;

                if (ll.index) |prev_elem_i| {
                    const prev_elem = ll.tree.get(prev_elem_i);
                    prev_elem.next_sibling = node_idx;
                } else {
                    ll.start_index = node_idx;
                }
                ll.index = node_idx;
                // ll.count += 1;

                // std.debug.print("append END:  {any}\n", .{ll.ed.data.items});
            }

            pub fn next(ll: *LinkedList) ?*Node {
                if (ll.index == null) return null;

                const node = ll.tree.get(ll.index.?);
                ll.index = if (node.next_sibling == 0) null else node.next_sibling;

                return node;
            }

            pub fn nextIdx(ll: *LinkedList) ?NodeIndex {
                if (ll.index == null) return null;
                const node_idx = ll.index.?;
                const node = ll.tree.get(node_idx);
                ll.index = if (node.next_sibling == 0) null else node.next_sibling;

                return node_idx;
            }
        }; // List

    };
}

test "Tree" {
    const DoenerTree = GenericTree(struct {
        fat_level: u32 = 0,
        first_child: NodeIndex = 0,
        next_sibling: NodeIndex = 0,
    });
    var t = try DoenerTree.init(std.testing.allocator, 10);
    defer t.deinit();

    const snapshot1 = try t.takeSnapshot();
    defer t.freeSnapshot(snapshot1);
    try std.testing.expect(t.equals(snapshot1));

    const index = try t.append(.{ .fat_level = 42 });
    try std.testing.expectEqual(t.get(index).fat_level, 42);
    try std.testing.expect(!t.equals(snapshot1));

    var list = t.startList();
    try list.append(.{ .fat_level = 1 });
    try list.append(.{ .fat_level = 2 });
    try list.append(.{ .fat_level = 3 });

    const snapshot2 = try t.takeSnapshot();
    defer t.freeSnapshot(snapshot2);
    try std.testing.expect(t.equals(snapshot2));

    t.get(index).first_child = list.start_index;

    {
        var ll = t.getList(index);
        try std.testing.expect(ll.next().?.fat_level == 42);
        try std.testing.expect(ll.next() == null);
        try std.testing.expect(ll.next() == null);
    }

    {
        var ll = t.getList(list.start_index);
        try std.testing.expect(ll.next().?.fat_level == 1);
        try std.testing.expect(ll.next().?.fat_level == 2);
        try std.testing.expect(ll.next().?.fat_level == 3);
        try std.testing.expect(ll.next() == null);
    }
}
