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
            .variable => |v| try writer.print("({s})", .{v}),
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

const Constraint = struct {
    lhs: TypeVar,
    rhs: TypeVar,

    pub fn format(self: *const Constraint, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;

        try writer.print("{} ~ {}", .{ self.lhs, self.rhs });
    }
};

const Analysis = struct {
    gpa: std.mem.Allocator,
    constraints: std.ArrayListUnmanaged(Constraint),
    subs: std.ArrayListUnmanaged(struct { lhs: TypeVar, rhs: TypeVar }),
    expr_type_vars: std.AutoHashMapUnmanaged(*const Expr, TypeVar),
    var_type_vars: std.StringHashMapUnmanaged(TypeVar),

    next_var: u32,

    fn init(gpa: std.mem.Allocator) Analysis {
        return .{
            .gpa = gpa,
            .expr_type_vars = .{},
            .var_type_vars = .{},
            .constraints = .{},
            .subs = .{},
            .next_var = 0,
        };
    }

    fn tyvar(self: *Analysis) TypeVar {
        var tv = TypeVar{ .variable = self.next_var };
        self.next_var += 1;
        return tv;
    }

    fn generateConstraints(self: *Analysis, expr: *const Expr) !TypeVar {
        const tv = self.tyvar();
        switch (expr.*) {
            .variable => |v| {
                try self.expr_type_vars.put(self.gpa, expr, tv);

                var gop = try self.var_type_vars.getOrPut(self.gpa, v);
                if (!gop.found_existing) gop.value_ptr.* = self.tyvar();
                try self.constraints.append(self.gpa, .{ .lhs = tv, .rhs = gop.value_ptr.* });
            },
            .literal => |l| try self.constraints.append(self.gpa, .{ .lhs = tv, .rhs = switch (l) {
                .int => .{ .concrete = .int },
                .boolean => .{ .concrete = .boolean },
                .string => .{ .concrete = .string },
            } }),
            .function => |f| {
                try self.var_type_vars.put(self.gpa, f.name, tv);

                var args = std.ArrayListUnmanaged(TypeVar){};

                for (f.params) |param| {
                    var p_tv = self.tyvar();
                    try args.append(self.gpa, p_tv);
                    try self.var_type_vars.put(self.gpa, param, p_tv);
                }

                var b_tv = try self.generateConstraints(f.body);
                try args.append(self.gpa, b_tv);

                try self.constraints.append(self.gpa, .{
                    .lhs = tv,
                    .rhs = .{
                        .concrete = .{ .construct = .{ .constructor = "->", .args = try args.toOwnedSlice(self.gpa) } },
                    },
                });
            },
            .apply => |a| {
                const f_tv = try self.generateConstraints(a.function);

                var args = std.ArrayListUnmanaged(TypeVar){};
                for (a.arguments) |arg| {
                    const a_tv = try self.generateConstraints(arg);
                    try args.append(self.gpa, a_tv);
                }

                const ret_tv = self.tyvar();
                try args.append(self.gpa, ret_tv);

                try self.constraints.append(self.gpa, .{ .lhs = tv, .rhs = ret_tv });
                try self.constraints.append(self.gpa, .{
                    .lhs = f_tv,
                    .rhs = .{
                        .concrete = .{ .construct = .{ .constructor = "->", .args = try args.toOwnedSlice(self.gpa) } },
                    },
                });
            },
        }

        return tv;
    }

    fn debug(self: *const Analysis) !void {
        std.debug.print("=============== VARS ===============\n", .{});
        {
            var it = self.var_type_vars.iterator();
            while (it.next()) |entry|
                std.debug.print("{s} : {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }

        {
            var it = self.expr_type_vars.iterator();
            while (it.next()) |entry|
                std.debug.print("{} : {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }

        std.debug.print("============ CONSTRAINT ============\n", .{});
        for (self.constraints.items) |constraint| {
            std.debug.print("{}\n", .{constraint});
        }
    }

    fn infer(self: *Analysis, exprs: []const *Expr) !void {
        for (exprs) |ex| {
            _ = try self.generateConstraints(ex);
            try self.debug();

            self.expr_type_vars.clearRetainingCapacity();
            self.var_type_vars.clearRetainingCapacity();
            self.constraints.clearRetainingCapacity();
        }
    }
};

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

    var sys = Analysis.init(a);
    try sys.infer(al.items);
}
