const std = @import("std");

const assert = std.debug.assert;

pub const ExtraIndex = u32;

pub const ExtraData = struct {
    const BaseType = u32;

    data: std.ArrayList(BaseType),
    gpa: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator, initial_capacity: usize) !ExtraData {
        return .{
            .data = if (initial_capacity == 0) .empty else try .initCapacity(gpa, initial_capacity),
            .gpa = gpa,
        };
    }

    pub fn deinit(ed: *ExtraData) void {
        ed.data.deinit(ed.gpa);
    }

    /// T must be default initializable!
    pub fn add(ed: *ExtraData, T: type) !struct { ptr: *T, index: ExtraIndex } {
        comptime assert(@alignOf(T) <= @alignOf(BaseType));
        // comptime assert(@sizeOf(T) % @sizeOf(BaseType) == 0);
        const num_elems = (@sizeOf(T) - 1) / @sizeOf(BaseType) + 1;

        const index: ExtraIndex = @intCast(ed.data.items.len);

        const slice = try ed.data.addManyAsSlice(ed.gpa, num_elems);
        const data_ptr: *T = @ptrCast(slice.ptr);

        switch (@typeInfo(T)) {
            .@"struct" => data_ptr.* = .{},
            else => data_ptr.* = 0, // for numbers // TODO: add other types here if required!
        }

        // std.debug.print("add: result index: {} len={}  capacity={}\n", .{ index, ed.data.items.len, ed.data.capacity });
        return .{ .ptr = data_ptr, .index = index };
    }

    pub fn get(ed: *const ExtraData, T: type, index: ExtraIndex) *T {
        // std.debug.print("get: {}\n", .{index});
        comptime assert(@alignOf(T) <= @alignOf(BaseType));
        const num_elems = (@sizeOf(T) - 1) / @sizeOf(BaseType) + 1;

        assert(index + num_elems <= ed.data.items.len);
        const ptr: *T = @ptrCast(&ed.data.items[index]);

        return ptr;
    }

    pub fn startList(ed: *ExtraData, DataType: type) List(DataType) {
        return List(DataType){ .ed = ed };
    }

    pub fn getList(ed: *const ExtraData, DataType: type, index: ExtraIndex) List(DataType) {
        return List(DataType){
            .ed = @constCast(ed),
            .index = index,
            .start_index = index,
        };
    }

    fn List(DataType: type) type {
        return struct {
            const Self = @This();

            ed: *ExtraData,
            index: ?ExtraIndex = null,
            start_index: ExtraIndex = undefined, // will be set when first element is added.
            count: u32 = 0, // only for writitng! will always be 0 when reading!

            pub fn append(l: *Self, data: DataType) !void {
                // std.debug.print("append START:{any}\n", .{l.ed.data.items});

                const elem_ptr_and_index = try l.ed.add(ListElem);
                const elem = elem_ptr_and_index.ptr;
                elem.data = data;
                // elem.next = 0;

                if (l.index) |prev_elem_i| {
                    const prev_elem = l.ed.get(ListElem, prev_elem_i);
                    prev_elem.next = elem_ptr_and_index.index;
                } else {
                    l.start_index = elem_ptr_and_index.index;
                }
                l.index = elem_ptr_and_index.index;
                l.count += 1;

                // std.debug.print("append END:  {any}\n", .{l.ed.data.items});
            }

            pub fn next(l: *Self) ?DataType {
                if (l.index == null) return null;

                const elem = l.ed.get(ListElem, l.index.?);
                l.index = if (elem.next == 0) null else elem.next;

                return elem.data;
            }

            pub fn nextPtr(l: *Self) ?*const DataType {
                if (l.index == null) return null;

                const elem = l.ed.get(ListElem, l.index.?);
                l.index = if (elem.next == 0) null else elem.next;

                return &elem.data;
            }

            const ListElem = struct {
                data: DataType = undefined,
                next: ExtraIndex = 0,
            };
        };
    } // List

}; // struct ExtraData

test "ExtraData basic" {
    var ed = try ExtraData.init(std.testing.allocator, 0);
    defer ed.deinit();

    const ptr_and_index = try ed.add(i16);
    ptr_and_index.ptr.* = 42;
    const index42 = ptr_and_index.index;

    const ptr_and_index2 = try ed.add(i16);
    ptr_and_index2.ptr.* = 997;
    const index997 = ptr_and_index2.index;

    try std.testing.expectEqual(42, ed.get(i16, index42).*);
    try std.testing.expectEqual(997, ed.get(i16, index997).*);
}

test "ExtraData List" {
    var ed = try ExtraData.init(std.testing.allocator, 0);
    defer ed.deinit();

    const Stuff = struct {
        a: u32,
        b: i16,
        c: u16 = 1,
        d: u32,
    };

    const Dings = i32;

    var write_list = ed.startList(Stuff);
    try write_list.append(.{ .a = 12345, .b = 1, .d = 0 });
    try std.testing.expect(write_list.start_index == 0);

    const first_elem_idx = write_list.start_index;

    {
        var read_list = ed.getList(Stuff, first_elem_idx);
        var count: u32 = 0;
        while (read_list.nextPtr()) |stuff| {
            try std.testing.expectEqual(12345, stuff.a);
            try std.testing.expectEqual(count, stuff.d);
            count += 1;
        }
        try std.testing.expectEqual(1, count);
        try std.testing.expect(read_list.start_index == first_elem_idx);
    }

    var write_list2 = ed.startList(Dings);
    try write_list2.append(42);
    const first_elem_idx2 = write_list2.start_index;
    try std.testing.expect(first_elem_idx2 > 0);

    try write_list2.append(4321);

    for (1..997) |d| {
        _ = try write_list.append(.{ .a = 12345, .b = 1, .d = @intCast(d) });
    }
    try std.testing.expect(write_list.start_index == first_elem_idx);

    {
        var read_list = ed.getList(Stuff, first_elem_idx);
        var count: u32 = 0;
        while (read_list.nextPtr()) |stuff| {
            try std.testing.expectEqual(12345, stuff.a);
            try std.testing.expectEqual(count, stuff.d);
            count += 1;
        }
        try std.testing.expectEqual(997, count);
        try std.testing.expect(read_list.start_index == first_elem_idx);
    }

    {
        var read_list = ed.getList(Dings, first_elem_idx2);
        var count: u32 = 0;
        while (read_list.next()) |dings| {
            switch (count) {
                0 => try std.testing.expectEqual(42, dings),
                1 => try std.testing.expectEqual(4321, dings),
                else => {},
            }
            count += 1;
        }
        try std.testing.expectEqual(2, count);
        try std.testing.expect(read_list.start_index == first_elem_idx2);
    }
}
