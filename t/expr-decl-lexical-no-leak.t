use Test;

# An expression-position `my` inside a routine (`if (my $d = ...)`,
# `while my $w = ...`) is the routine's own fresh binding. It must never leak
# back into a same-named lexical of the CALLER — through any of the return
# paths: the compiled scoped-overlay merge, the method-dispatch merge, the
# free-var writeback drain, or the interpreter-carrier writeback log.
# Found via Text::CSV: `method csv`'s `if (my $file = %args<file> :delete)`
# clobbered the caller script's `$file` with Any.

plan 8;

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
# NOTE: the caller lexical is declared AFTER the sub on purpose. Declared
# before, the sub CAPTURES it, and the declaration's env store writes through
# the captured cell — a separate, still-open leak:
# todo/deep/expr-decl-writes-through-captured-cell.md
sub h-carrier(*%args) { if (my $g = %args<g> :delete) { return 1 }; 42 }
my $g = "outer";
EVAL q[h-carrier()];
is $g, "outer", 'if-cond my under an EVAL carrier does not clobber caller lexical';

# --- the declaration still works as a value ---
sub h-value() { if (my $v = 7) { $v + 1 } else { 0 } }
is h-value(), 8, 'the condition binding is usable inside the branch';
