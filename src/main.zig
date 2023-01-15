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
    function: struct { name: []const u8, params: []const Parameter, body: *Expr },
    apply: struct { function: *Expr, arguments: []const *Expr },

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

const ExprBuilder = struct {
    gpa: std.mem.Allocator,

    fn e(self: *ExprBuilder, expr: Expr) !*Expr {
        var ex = try self.gpa.create(Expr);
        ex.* = expr;
        return ex;
    }

    fn lit(self: *ExprBuilder, l: Literal) !*Expr {
        return self.e(.{ .literal = l });
    }

    fn variable(self: *ExprBuilder, s: []const u8) !*Expr {
        return self.e(.{ .variable = s });
    }

    fn function(self: *ExprBuilder, name: []const u8, params: []const Parameter, body: *Expr) !*Expr {
        return self.e(.{ .function = .{ .name = name, .params = params, .body = body } });
    }

    fn apply(self: *ExprBuilder, func: *Expr, arguments: []const *Expr) !*Expr {
        return self.e(.{ .apply = .{ .function = func, .arguments = arguments } });
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

    var b = ExprBuilder{ .gpa = a };

    var al = std.ArrayListUnmanaged(*Expr){};
    try al.append(a, try b.function("id", &[1][]const u8{"x"}, try b.variable("x")));
    try al.append(a, try b.apply(try b.variable("id"), &[1]*Expr{try b.lit(.{ .int = 5 })}));

    for (al.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }
}
