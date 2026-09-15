use v6;
use Test;

# A user-defined `infix:<op>` sub can declare either parameter `is rw`
# (French's `my &infix:<plus_égal> = sub ($a is rw, $b) { $a += $b };`,
# called bareword-style as `$x plus_égal 5`). `Compiler::compile_expr_infix_func`
# compiled both operands with a plain `compile_expr`, which always leaves a
# bare VALUE on the stack — only `compile_call_arg` attaches the `WrapVarRef`
# metadata a variable operand needs to bind an `is rw` parameter to the
# caller's own container, exactly as an ordinary function call argument
# already does. Without it every such call died "Parameter '$a' expects a
# writable container (variable) as an argument, but got ... as a value
# without a container" — the ecosystem `French` distribution's assignment
# operators (`plus_égal`/`moins_égal`/`fois_égal`) hit this.

plan 9;

my &infix:<plus-eq> = sub ($a is rw, $b) { $a += $b };
my $x = 10;
$x plus-eq 5;
is $x, 15, 'a custom infix with an `is rw` LEFT parameter writes through';

my &infix:<eq-plus> = sub ($a, $b is rw) { $b += $a };
my $y = 10;
3 eq-plus $y;
is $y, 13, 'and an `is rw` RIGHT parameter too';

# CONTROL: an ordinary (non-rw) custom infix is unaffected.
my &infix:<add> = sub ($a, $b) { $a + $b };
is (4 add 5), 9, 'a plain custom infix still returns its value, unaffected';

# CONTROL: the parenthesized call spelling already worked (it goes through
# an unrelated compile path) and must keep working.
my &infix:<plus-eq2> = sub ($a is rw, $b) { $a += $b };
my $z = 1;
&infix:<plus-eq2>($z, 9);
is $z, 10, 'the parenthesized-call spelling of the same operator still writes through';

# CONTROL: a non-variable left operand (nothing to write back to) still just
# raises the ordinary rw-container error, not something new.
my &infix:<bump> = sub ($a is rw, $b) { $a += $b };
dies-ok { 5 bump 1 }, 'a literal LEFT operand still refuses the `is rw` bind';

# CONTROL: passing a `compile_call_arg`-wrapped variable operand THROUGH to a
# native word operator (never `is rw` itself) must still see the plain value,
# not the wrapping. `word-compound-assign` desugars `$m mod= 5` into an
# `InfixFunc` node whose LEFT operand is the SAME variable the outer
# assignment targets (`$m = $m mod 5`) -- a first attempt at this fix left
# the wrap on all the way into the native `mod`/`×`/`÷` dispatch, which reads
# an un-derefed tag as an unmatched type and silently defaults instead of
# computing the real value (`$m mod= 5` computed `0 mod 5`, not `17 mod 5`).
{
    my $m = 17;
    $m mod= 5;
    is $m, 2, 'a native word operator (mod=) is unaffected by the is-rw fix';
}
{
    my $m = 12;
    $m div= 5;
    is $m, 2, 'div= too, for good measure (a different compile path already)';
}

# CONTROL: the Unicode arithmetic aliases (`×`/`÷`), which have their own
# native dispatch helper reached from the SAME `InfixFunc` opcode.
{
    my $p = 6;
    my $q = $p × 7;
    is $q, 42, 'the Unicode × alias still computes correctly with a variable operand';
}
{
    my $r = 20;
    my $s = $r ÷ 4;
    is $s, 5, 'and ÷';
}
