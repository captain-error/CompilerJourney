const std = @import("std");

const assert = std.debug.assert;

pub fn FixedSizeStack(comptime T: type, comptime capacity : comptime_int) type {
    return struct {
        items: [capacity]T = [_]T{.{}}**capacity,
        size: u32 = 0,

        pub fn clear(self: *FixedSizeStack(T, capacity)) void {
            self.size = 0;
        }

        pub fn push_without_init(self: *FixedSizeStack(T, capacity)) !void {
            if (self.size >= capacity) 
                return error.FixedSizeStackOverflow;
            self.size += 1;
        }

        pub fn pop(self: *FixedSizeStack(T, capacity)) *T {
            assert (self.size > 0);
            self.size -= 1;
            return &self.items[self.size];
        }

        pub fn peek(self: *FixedSizeStack(T, capacity), depth: u32) *T {
            assert (depth < self.size);
            return &self.items[self.size - 1 - depth];
        }

        pub fn top(self: *FixedSizeStack(T, capacity)) *T {
            assert (self.size > 0);
            return &self.items[self.size - 1];
        }
    };
}

pub fn DynamicStack(comptime T: type) type {
    return struct {
        _array_list: std.ArrayList(T) = .empty,

        pub const Iterator = struct {
            stack : *DynamicStack(T),
            index : usize,

            pub fn next(self: *Iterator) ?*T {
                if (self.index == 0)
                    return null;
                self.index -= 1;
                return &self.stack._array_list.items[self.index];
            }
        };

        pub fn deinit(self: *DynamicStack(T), gpa : std.mem.Allocator) void {
            self._array_list.deinit(gpa);
        }

        pub fn clear(self: *DynamicStack(T)) void {
            self._array_list.clearRetainingCapacity();
        }

        pub fn size(self: *const DynamicStack(T)) usize {
            return self._array_list.items.len;
        }

        pub fn push(self: *DynamicStack(T), gpa : std.mem.Allocator, item: T) !void {
            try self._array_list.append(gpa, item);
        }

        pub fn pop(self: *DynamicStack(T)) ?*T {
            assert(self.size() > 0);
            return self._array_list.pop();
        }

        pub fn peek(self: *DynamicStack(T), depth: u32) *T {
            assert(depth < self.size());
            return &self._array_list.items[self.size() - 1 - depth];
        }

        pub fn top(self: *DynamicStack(T)) *T {
            assert(self.size() > 0);
            return &self._array_list.items[self.size() - 1];
        }

        pub fn iterator(self: *DynamicStack(T)) Iterator {
            return Iterator{ .stack = self, .index = self.size() };
         }
    };
}

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