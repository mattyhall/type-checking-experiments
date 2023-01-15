const std = @import("std");

pub const Literal = union(enum) {
    int: i64,
    boolean: bool,
    string: []const u8,

    pub fn format(self: *const Literal, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            inline .int, .boolean => |v| try writer.print("{}", .{v}),
            .string => |s| try writer.writeAll(s),
        }
    }
};

pub const Parameter = []const u8;

pub const Expr = union(enum) {
    variable: []const u8,
    literal: Literal,
    function: struct { name: []const u8, params: []Parameter, body: *Expr },
    apply: struct { function: *Expr, arguments: []*Expr },

    pub fn format(self: *const Expr, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .variable => |v| try writer.writeAll(v),
            .literal => |l| try writer.print("{}", .{l}),
            .function => |f| {
                try writer.print("(fn {s} [", .{f.name});
                for (f.params) |param, i| {
                    try writer.writeAll(param);
                    if (i < f.params.len - 1) try writer.writeAll(" ");
                }
                try writer.print("] {})", .{f.body});
            },
            .apply => |a| {
                try writer.print("({} ", .{a.function});
                for (a.arguments) |arg, i| {
                    try writer.print("{}", .{arg});
                    if (i < a.arguments.len - 1) try writer.writeAll(" ");
                }
                try writer.writeAll(")");
            },
        }
    }
};

pub const Type = union(enum) {
    int,
    boolean,
    string,
    construct: struct { constructor: []const u8, args: []TypeVar },

    pub fn format(self: *const Type, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .int => try writer.writeAll("Int"),
            .boolean => try writer.writeAll("Bool"),
            .string => try writer.writeAll("String"),
            .construct => |c| {
                try writer.print("({s} ", .{c.constructor});
                for (c.args) |arg, i| {
                    try writer.print("{}", .{arg});
                    if (i < c.args.len - 1) try writer.writeAll(" ");
                }
                try writer.writeAll(")");
            },
        }
    }
};

pub const TypeVar = union(enum) {
    concrete: Type,
    variable: u32,

    pub fn format(self: *const TypeVar, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .concrete => |t| try writer.print("{}", .{t}),
            .variable => |u| try writer.print("t{}", .{u}),
        }
    }
};

pub const Constraint = union(enum) { eq: struct { lhs: TypeVar, rhs: TypeVar } };

pub const Subsitutions = std.AutoHashMap(TypeVar, TypeVar);

fn infer(alloc: std.mem.Allocator, stmts: []Expr) !void {
    _ = stmts;
    _ = alloc;
}

fn e(alloc: std.mem.Allocator, expr: Expr) !*Expr {
    var new = try alloc.create(Expr);
    new.* = expr;
    return new;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    arena.deinit();

    var a = arena.allocator();

    var params: [1][]const u8 = [1][]const u8{"x"};
    var args: [1]*Expr = [1]*Expr{try e(a, Expr{ .literal = .{ .int = 4 } })};

    var al = std.ArrayListUnmanaged(Expr){};
    try al.append(a, Expr{ .function = .{ .name = "id", .params = &params, .body = try e(a, Expr{ .variable = "x" }) } });
    try al.append(a, Expr{ .apply = .{ .function = try e(a, Expr{ .variable = "id" }), .arguments = &args } });

    for (al.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }
}
