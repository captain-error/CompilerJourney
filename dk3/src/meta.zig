const std = @import("std");

pub fn containsField(Struct: type, comptime name: []const u8) bool {
    for (@typeInfo(Struct).@"struct".fields) |field|
        if (std.mem.eql(u8, field.name, name)) return true;

    return false;
}
