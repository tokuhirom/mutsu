use Test;

# A `warn` raised deep inside a sub/method call chain, with NO CONTROL handler
# installed, must print to stderr and *resume* — the suspended computation
# continues and its value flows back out. Previously mutsu unwound the Rust call
# stack on the warn signal, abandoning every frame between the warn and the
# top-level loop (cross-frame warn), so the deep result was lost.

plan 6;

# Direct `warn` two frames deep.
sub inner-direct() { warn "deep direct"; return "inner-result"; }
sub outer-direct() { my $x = inner-direct(); return "outer[$x]"; }
is outer-direct(), "outer[inner-result]",
    'direct warn deep in call chain resumes; caller value preserved';

# Indirect `&warn.(...)` two frames deep (the Template::Mustache logger shape).
sub inner-indirect() { my $w = &warn; $w.("deep indirect"); return "inner-ok"; }
sub outer-indirect() { return "outer[" ~ inner-indirect() ~ "]"; }
is outer-indirect(), "outer[inner-ok]",
    'indirect &warn.() deep in call chain resumes; caller value preserved';

# warn inside a method call chain resumes too.
class C {
    method deep() { warn "method warn"; return 42; }
    method top()  { return self.deep() + 1; }
}
is C.new.top(), 43, 'warn inside a method chain resumes and the value flows out';

# warn mid-expression resumes; the rest of the expression still evaluates.
sub w-mid() { return 1 + (warn("mid") // 0) + 2; }
is w-mid(), 3, 'warn mid-expression resumes (value is Nil) and expression completes';

# A loop body that warns keeps iterating (one warn per element, all resume).
my @collected;
for 1..3 -> $n { warn "warn $n"; @collected.push: $n * 10; }
is @collected, [10, 20, 30], 'warn in a loop body resumes every iteration';

# `quietly` suppresses the warning output but execution still resumes.
sub quiet-deep() { quietly { warn "hushed"; }; return "after-quiet"; }
is quiet-deep(), "after-quiet", 'warn under quietly resumes without aborting';
