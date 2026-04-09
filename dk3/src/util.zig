const std = @import("std");

fn IndexRange_(IndexType : type) type {
    return struct {
        start: IndexType,
        end: IndexType,

        const Self = @This();

        pub const Iterator = struct {
            index: usize,
            end: usize,

            pub fn next(self: *Iterator) ?IndexType {
                if (self.index >= self.end) return null;
                const result:IndexType = @enumFromInt( self.index);
                self.index += 1;
                return result;
             }
        };

        pub fn iterator(self: Self) Iterator {
            return Iterator{ .index = @intFromEnum(self.start), .end = @intFromEnum(self.end) };
        }

        pub fn len(self: Self) usize {
            return @as(usize,@intFromEnum(self.end)) - @as(usize, @intFromEnum(self.start));
         }
    };
}

/// An ArrayList with a distinct index type.
pub fn ArrayList(comptime T: type) type {
    return struct {
        items: [*]T,
        len: usize = 0,
        capacity: usize = 0,
        gpa: std.mem.Allocator,

        const Index = enum(u31) { NONE = 0, _,
            pub fn plus(self: Index, summand: anytype) Index {
                return @enumFromInt(self.idx()+summand);
            }

            pub fn idx(self: Index) usize {
                return @intFromEnum(self);
            }  

            
        };

        const IndexRange = IndexRange_(Index);
        const empty_range = IndexRange{ .start = .NONE, .end = .NONE }; 


        pub fn slice(self: *const ArrayList(T), index: Index, len : usize) []const T {
            return self.items[@intFromEnum(index) .. @intFromEnum(index) + len];
        }

        pub fn sliceFromRange(self: *const ArrayList(T), range: IndexRange) []const T {
            const start_idx = @intFromEnum(range.start);
            const end_idx = @intFromEnum(range.end);
            std.debug.assert(start_idx <= end_idx);
            std.debug.assert(end_idx <= self.len);
            return self.items[start_idx .. end_idx];
        }

        pub fn get(self: *const ArrayList(T), index : Index) T {
            std.debug.assert(index != .NONE);
            const idx = @intFromEnum(index);
            std.debug.assert(idx < self.len);
            return self.items[idx];
        }

        pub fn append(self: *ArrayList(T), item: T) !Index {
            if (self.len >= self.items.len) {
                const new_items = try self.gpa.realloc(self.items[0..self.capacity], self.capacity * 2);
                self.items = new_items.ptr;
                self.capacity = new_items.len;
            }

            self.items[self.len] = item;
            const idx : Index = @enumFromInt(self.len);
            self.len += 1;
            return idx;
        }

        pub fn init(gpa: std.mem.Allocator, initial_size: usize) !ArrayList(T) {
            var items = try gpa.alloc(T, initial_size+1);
            items[0] = .{}; // dummy item at index 0 so that we can use .NONE as a sentinel value for "no item"
            return ArrayList(T){ .items = items.ptr, .capacity = items.len, .gpa = gpa, .len = 1 };
        }
    };
}