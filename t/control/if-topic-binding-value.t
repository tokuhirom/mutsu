use v6;
use Test;

# An `if EXPR -> $x { ... }` / pointy `elsif` used in value position must yield
# the taken branch's value (not Nil). Previously the topic-binding form fell
# through to a Nil result because the value-if compiler paths guarded on
# `binding_var.is_none()`.

plan 12;

# --- if with pointy topic, as a sub's implicit return value ---
sub a($x) { if $x -> $y { "got:$y" } }
is a("A"), "got:A", "if EXPR -> \$y { } returns branch value";
nok a("").Bool, "if EXPR -> \$y { } false cond yields a falsy (empty) result";

# --- elsif with pointy topic ---
sub b($x) {
    if False { "no" }
    elsif $x -> $y { "eb:$y" }
}
is b("C"), "eb:C", "elsif EXPR -> \$y { } returns branch value";

# --- if/elsif/else chain, elsif taken ---
sub c($x) {
    if $x.starts-with('.') { "dot" }
    elsif $x.uc -> $u { "uc:$u" }
    else { "else" }
}
is c("abc"), "uc:ABC", "elsif pointy in full chain returns branch value";
is c(".x"),  "dot",    "then branch still wins when its cond is true";

# --- topic variable is actually bound to the condition value ---
sub d($n) { if $n * 2 -> $doubled { $doubled + 1 } }
is d(5), 11, "bound topic holds the condition value";

# --- nested elsif chain, second elsif taken ---
sub e($x) {
    if False { "a" }
    elsif False { "b" }
    elsif $x -> $v { "c:$v" }
}
is e("Z"), "c:Z", "second pointy elsif returns branch value";

# --- value position inside do { } ---
my $r = do if 7 -> $z { $z * 3 };
is $r, 21, "do if EXPR -> \$z { } yields branch value";

# --- as the value of a `for` body element ---
my @out = (for 1..3 -> $i { if $i %% 2 -> $ok { "even:$i" } else { "odd:$i" } });
is @out.join(","), "odd:1,even:2,odd:3", "if pointy inside for body collects values";

# --- multi-statement then branch: last value flows out ---
sub f($x) {
    if $x -> $y {
        my $tmp = $y ~ "!";
        $tmp ~ "?";
    }
}
is f("hi"), "hi!?", "multi-statement pointy then-branch returns its last value";

# --- plain (non-pointy) if still works (no regression) ---
sub g($x) { if $x { "plain:$x" } }
is g("P"), "plain:P", "plain if (no pointy) still returns branch value";

# --- with (already worked) as a control ---
sub h($x) { with $x -> $y { "wb:$y" } }
is h("W"), "wb:W", "with EXPR -> \$y { } still returns branch value";
