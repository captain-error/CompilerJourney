const std = @import("std");

const assert = std.debug.assert;


fn IndexRange_(IndexType : type) type {
    return struct {
        start: IndexType,
        end: IndexType,

        const Self = @This();

        pub fn create(start: usize, length: usize) Self {
            return Self{ .start = @enumFromInt(start), .end = @enumFromInt(start + length) };
        }

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
        items: []T,
        capacity: usize = 0,
        gpa: std.mem.Allocator,

        pub const Index = enum(u31) { NONE = 0, _,
            pub fn plus(self: Index, summand: anytype) Index {
                return @enumFromInt(self.idx()+summand);
            }

            pub fn idx(self: Index) usize {
                return @intFromEnum(self);
            }  

            
        };

        pub const IndexRange = IndexRange_(Index);
        pub const empty_range = IndexRange{ .start = .NONE, .end = .NONE }; 




        pub fn slice(self: *const ArrayList(T), index: Index, len : usize) []const T {
            return self.items[@intFromEnum(index) .. @intFromEnum(index) + len];
        }

        pub fn sliceFromRange(self: *const ArrayList(T), range: IndexRange) []const T {
            const start_idx = @intFromEnum(range.start);
            const end_idx = @intFromEnum(range.end);
            assert(start_idx <= end_idx);
            assert(end_idx <= self.items.len);
            return self.items[start_idx .. end_idx];
        }

        pub fn get(self: *const ArrayList(T), index : Index) T {
            assert(index != .NONE);
            const idx = @intFromEnum(index);
            assert(idx < self.items.len);
            return self.items[idx];
        }

        pub fn append(self: *ArrayList(T), item: T) !Index {
            if (self.items.len >= self.capacity) {
                const new_items = try self.gpa.realloc(self.items[0..self.capacity], self.capacity * 2);
                self.items = new_items[0..self.items.len];
                self.capacity = new_items.len;
                assert(self.items.len < self.capacity);
            }

            const idx : Index = @enumFromInt(self.items.len);
            self.items.ptr[self.items.len] = item;
            self.items.len += 1;
            return idx;
        }

        pub fn ensureCapacity(self: *ArrayList(T), capacity: usize) !void {
            if (capacity > self.capacity) {
                const new_items = try self.gpa.realloc(self.items[0..self.capacity], capacity);
                self.items = new_items[0..self.items.len];
                self.capacity = new_items.len;
            }
        }

        pub fn init(gpa: std.mem.Allocator, initial_size: usize) !ArrayList(T) {
            var items = try gpa.alloc(T, initial_size+1);
            items[0] = .{}; // dummy item at index 0 so that we can use .NONE as a sentinel value for "no item"
            return ArrayList(T){ .items = items[0..1], .capacity = items.len, .gpa = gpa };
        }

        pub fn initWithNullElement(gpa: std.mem.Allocator, initial_size: usize, nullElement : T) !ArrayList(T) {
            var items = try gpa.alloc(T, initial_size+1);
            items[0] = nullElement; // dummy item at index 0 so that we can use .NONE as a sentinel value for "no item"
            return ArrayList(T){ .items = items[0..1], .capacity = items.len, .gpa = gpa };
        }

        pub fn deinit(self: *ArrayList(T)) void {
            self.gpa.free(self.items.ptr[0..self.capacity]);
        }
    };
}