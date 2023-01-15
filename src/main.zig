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

pub const TypeOrGeneric = union(enum) {
    concrete: Type,
    generic: u32,

    pub fn format(self: *const TypeOrGeneric, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self) {
            .concrete => |t| try writer.print("{}", .{t}),
            .generic => |n| try writer.print("g{}", .{n}),
        }
    }
};

pub const ConstType = enum {
    int,
    boolean,
    string,

    pub fn format(self: *const ConstType, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .int => try writer.writeAll("Int"),
            .boolean => try writer.writeAll("Bool"),
            .string => try writer.writeAll("String"),
        }
    }
};

pub const Type = union(enum) {
    constant: ConstType,
    construct: struct { constructor: []const u8, args: []TypeOrGeneric },

    fn eql(lhs: Type, rhs: Type) bool {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        switch (lhs) {
            .constant => |t| return t == rhs.constant,
            .construct => |c| {
                if (!std.mem.eql(u8, c.constructor, rhs.construct.constructor)) return false;

                if (c.args.len != rhs.construct.args.len) return false;

                for (c.args) |lhs_arg, i| {
                    const rhs_arg = rhs.construct.args[i];
                    if (!lhs_arg.eql(rhs_arg)) return false;
                }

                return true;
            },
        }
    }

    pub fn format(self: *const Type, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .constant => |t| try writer.print("{}", .{t}),
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
    constant: ConstType,
    construct: struct { constructor: []const u8, args: []TypeVar },
    variable: u32,

    fn eql(lhs: TypeVar, rhs: TypeVar) bool {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        return switch (lhs) {
            .constant => |c| c == rhs.constant,
            .construct => |c| {
                if (!std.mem.eql(u8, c.constructor, rhs.construct.constructor)) return false;

                if (c.args.len != rhs.construct.args.len) return false;
                for (c.args) |lhs_arg, i| {
                    const rhs_arg = rhs.construct.args[i];
                    if (!lhs_arg.eql(rhs_arg)) return false;
                }

                return true;
            },
            .variable => |v| v == rhs.variable,
        };
    }

    pub fn format(self: *const TypeVar, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .constant => |t| try writer.print("{}", .{t}),
            .construct => |c| {
                try writer.print("({s} ", .{c.constructor});
                for (c.args) |arg, i| {
                    try writer.print("{}", .{arg});
                    if (i < c.args.len - 1) try writer.writeAll(" ");
                }
                try writer.writeAll(")");
            },
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
    debug: bool = false,
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
                .int => .{ .constant = .int },
                .boolean => .{ .constant = .boolean },
                .string => .{ .constant = .string },
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
                        .construct = .{ .constructor = "->", .args = try args.toOwnedSlice(self.gpa) },
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
                        .construct = .{ .constructor = "->", .args = try args.toOwnedSlice(self.gpa) },
                    },
                });
            },
        }

        return tv;
    }

    fn printVarsAndConstraints(self: *const Analysis) !void {
        std.debug.print("####################################\n", .{});
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

    fn printSubs(self: *const Analysis) void {
        std.debug.print("[", .{});
        for (self.subs.items) |s, i| {
            std.debug.print("{} := {}", .{ s.lhs, s.rhs });
            if (i < self.subs.items.len - 1) std.debug.print(", ", .{});
        }
        std.debug.print("]", .{});
    }

    fn printAddSub(self: *const Analysis, constraint: *const Constraint, to_be_replaced: TypeVar, with: TypeVar) void {
        std.debug.print("{}: Adding {} := {} to ", .{ constraint, to_be_replaced, with });
        self.printSubs();
        std.debug.print("\n", .{});
    }

    fn replaceTypeVar(self: *Analysis, tv: *TypeVar, to_be_replaced: u32, with: TypeVar) bool {
        switch (tv.*) {
            .variable => |v| if (v == to_be_replaced) {
                tv.* = with;
                return true;
            } else {},
            .constant => {},
            .construct => |c| {
                var ret = false;
                for (c.args) |*arg| {
                    ret = ret or self.replaceTypeVar(arg, to_be_replaced, with);
                }
                return ret;
            },
        }

        return false;
    }

    fn replace(self: *Analysis, to_be_replaced: u32, with: TypeVar) void {
        for (self.constraints.items) |*constraint| {
            var orig = constraint.*;

            if ((self.replaceTypeVar(&constraint.lhs, to_be_replaced, with) or
                self.replaceTypeVar(&constraint.rhs, to_be_replaced, with)) and self.debug)
            {
                if (self.debug) std.debug.print("  {} => {}\n", .{ orig, constraint.* });
            }
        }
    }

    fn pruneConstraints(self: *Analysis) void {
        var i: usize = 0;
        while (i < self.constraints.items.len) {
            const constraint = self.constraints.items[i];
            if (constraint.lhs.eql(constraint.rhs)) {
                std.debug.print("{}: Removing\n", .{constraint});
                _ = self.constraints.swapRemove(i);
                continue;
            }

            i += 1;
        }
    }

    fn solve(self: *Analysis) !void {
        if (self.debug) std.debug.print("============ UNIFICATION ===========\n", .{});

        var made_progress = true;
        while (made_progress) {
            made_progress = false;

            var to_add = std.ArrayListUnmanaged(Constraint){};

            var i: usize = 0;
            while (i < self.constraints.items.len) : (i += 1) {
                var constraint = self.constraints.items[i];
                if (constraint.lhs == .variable) {
                    if (self.debug) self.printAddSub(&constraint, constraint.lhs, constraint.rhs);

                    self.replace(constraint.lhs.variable, constraint.rhs);
                    try self.subs.append(self.gpa, .{ .lhs = constraint.lhs, .rhs = constraint.rhs });
                    made_progress = true;
                    continue;
                }

                if (constraint.rhs == .variable) {
                    if (self.debug) self.printAddSub(&constraint, constraint.rhs, constraint.lhs);

                    self.replace(constraint.rhs.variable, constraint.lhs);
                    try self.subs.append(self.gpa, .{ .lhs = constraint.rhs, .rhs = constraint.lhs });
                    made_progress = true;
                    continue;
                }

                if (constraint.lhs != .construct or constraint.rhs != .construct) continue;

                var lhs = constraint.lhs.construct;
                var rhs = constraint.rhs.construct;
                if (!std.mem.eql(u8, lhs.constructor, rhs.constructor)) continue;

                if (lhs.args.len != rhs.args.len) continue;

                if (self.debug) std.debug.print("{}: Expanding\n", .{constraint});

                for (lhs.args) |lhs_arg, j| {
                    var rhs_arg = rhs.args[j];
                    try to_add.append(self.gpa, .{ .lhs = lhs_arg, .rhs = rhs_arg });
                    if (self.debug) std.debug.print("  {} ~ {}\n", .{ lhs_arg, rhs_arg });
                }

                made_progress = true;
            }

            for (to_add.items) |c| {
                try self.constraints.append(self.gpa, c);
            }

            self.pruneConstraints();
        }
    }

    fn infer(self: *Analysis, exprs: []const *Expr) !void {
        for (exprs) |ex| {
            const res = try self.generateConstraints(ex);
            _ = res;

            if (self.debug) try self.printVarsAndConstraints();

            try self.solve();

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
    // try al.append(a, try b.apply(try b.variable("id"), &[1]*Expr{try b.lit(.{ .int = 5 })}));

    for (al.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }

    var sys = Analysis.init(a);
    sys.debug = true;
    try sys.infer(al.items);
}
