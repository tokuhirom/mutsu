use Test;

# An expression-position `my` inside a routine (`if (my $d = ...)`,
# `while my $w = ...`) is the routine's own fresh binding. It must never leak
# back into a same-named lexical of the CALLER — through any of the return
# paths: the compiled scoped-overlay merge, the method-dispatch merge, the
# free-var writeback drain, or the interpreter-carrier writeback log.
# Found via Text::CSV: `method csv`'s `if (my $file = %args<file> :delete)`
# clobbered the caller script's `$file` with Any.

plan 16;

# --- plain sub, if-condition declaration ---
sub h-if() { if (my $d = 5) { 1 }; 42 }
my $d = "outer";
h-if();
is $d, "outer", 'if-cond my in a sub does not clobber caller lexical';

# --- plain sub, condition declares Any (the Text::CSV shape) ---
sub h-any(*%args) { if (my $e = %args<e> :delete) { return 1 }; 42 }
my $e = "outer";
h-any();
is $e, "outer", 'if-cond my of a missing named arg stays callee-local';
is h-any(e => "x"), 1, 'the callee still sees its own condition value';

# --- plain sub, while-condition declaration ---
sub h-while() { while (my $w = 0) { }; 42 }
my $w = "outer";
h-while();
is $w, "outer", 'while-cond my in a sub does not clobber caller lexical';

# --- method dispatch path ---
class LeakProbe {
    method m(*%args) { if (my $f = %args<f> :delete) { return 1 }; 42 }
}
my $f = "outer";
LeakProbe.m();
is $f, "outer", 'if-cond my in a method does not clobber caller lexical';
is LeakProbe.m(f => "x"), 1, 'the method still sees its own condition value';

# --- interpreter-carrier path (EVAL logs by-name env writes) ---
# NOTE: the caller lexical is declared AFTER the sub on purpose (see below for
# the "declared before" shape, which is now covered too).
sub h-carrier(*%args) { if (my $g = %args<g> :delete) { return 1 }; 42 }
my $g = "outer";
EVAL q[h-carrier()];
is $g, "outer", 'if-cond my under an EVAL carrier does not clobber caller lexical';

# --- the declaration still works as a value ---
sub h-value() { if (my $v = 7) { $v + 1 } else { 0 } }
is h-value(), 8, 'the condition binding is usable inside the branch';

# --- method body + shared cell (the caller lexical is declared BEFORE the
# callee, and an escaping closure forces it into a shared `ContainerRef`
# cell). A class/role method's `CompiledCode` is registered separately from
# its enclosing frame, so it never appears in that frame's
# `closure_compiled_codes` — none of the `expr_declared_syms`-based
# protections above (which key off that list) ever ran for a method body,
# so every method-dispatch flavour leaked independently of the sub shapes
# above. Fixed by consulting `expr_declared_syms` directly at the two
# ContainerRef write-through sites (`resolved via
# todo/tickets/expr-decl-writes-through-captured-cell.md`). Covers all 8
# method-dispatch shapes that were found leaking.

# class method
my $a1 = "A1";
my $keep1 = sub { $a1 = $a1 };
class C1 { method m() { if (my $a1 = 0) { }; 42 } }
C1.m();
is $a1, "A1", 'if-cond my in a class method does not clobber a shared-cell caller lexical';

# role method
my $a2 = "A2";
my $keep2 = sub { $a2 = $a2 };
role R2 { method m() { if (my $a2 = 0) { }; 42 } }
class C2 does R2 { }
C2.new.m();
is $a2, "A2", 'if-cond my in a role method does not clobber a shared-cell caller lexical';

# submethod
my $a3 = "A3";
my $keep3 = sub { $a3 = $a3 };
class C3 { submethod sm() { if (my $a3 = 0) { }; 42 } }
C3.new.sm();
is $a3, "A3", 'if-cond my in a submethod does not clobber a shared-cell caller lexical';

# instance method
my $a4 = "A4";
my $keep4 = sub { $a4 = $a4 };
class C4 { method m() { if (my $a4 = 0) { }; 42 } }
C4.new.m();
is $a4, "A4", 'if-cond my in an instance method does not clobber a shared-cell caller lexical';

# multi method
my $a5 = "A5";
my $keep5 = sub { $a5 = $a5 };
class C5 {
    multi method m(Int $x) { if (my $a5 = 0) { }; 42 }
    multi method m() { if (my $a5 = 0) { }; 43 }
}
C5.new.m();
is $a5, "A5", 'if-cond my in a multi method does not clobber a shared-cell caller lexical';

# private method
my $a6 = "A6";
my $keep6 = sub { $a6 = $a6 };
class C6 {
    method pub() { self!priv() }
    method !priv() { if (my $a6 = 0) { }; 42 }
}
C6.new.pub();
is $a6, "A6", 'if-cond my in a private method does not clobber a shared-cell caller lexical';

# method invoked from a .map block
my $a7 = "A7";
my $keep7 = sub { $a7 = $a7 };
class C7 { method m() { if (my $a7 = 0) { }; 42 } }
(1, 2, 3).map({ C7.m() });
is $a7, "A7", 'if-cond my in a method called from .map does not clobber a shared-cell caller lexical';

# method invoked through an intermediate sub
my $a8 = "A8";
my $keep8 = sub { $a8 = $a8 };
class C8 { method m() { if (my $a8 = 0) { }; 42 } }
sub call-m() { C8.m() }
call-m();
is $a8, "A8", 'if-cond my in a method called via an intermediate sub does not clobber a shared-cell caller lexical';
