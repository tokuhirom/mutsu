use lib 't/lib';
use Test;
use FreeVarStoreTyped;

# A routine's assignment to its own free variable is type-checked against THAT
# variable, never against a same-named typed `my` of the calling scope (#10049).
#
# The value lane already resolved lexically (the write landed in the right
# variable), but the constraint lane read the name-keyed `__mutsu_type::` entry
# out of `env` -- and a callee's env is a child of its caller's, so a caller's
# `my Str $x` made the callee's write to an untyped outer `$x` die. The store
# now takes its constraint from the cell it lands in (ADR-0042), which is also
# what keeps a typed outer scalar enforced when no caller shadows it.
#
# Every row measured against raku v2026.07.

plan 13;

# 1-3: an untyped outer, a typed caller shadow.
my $x = 'outer';
my $seen;
sub init-x() { $seen = $x; $x = 42 }
{
    my Str $x = 'a';
    lives-ok { init-x() }, "caller's typed shadow does not constrain the callee's write";
    is $x, 'a', "caller's shadow keeps its own value";
}
is $x, 42, 'the write landed in the outer variable';

# 4: a typed outer is still enforced from a routine.
my Int $typed = 1;
sub set-typed($v) { $typed = $v }
throws-like { set-typed('s') }, X::TypeCheck::Assignment,
    'a typed outer scalar keeps its constraint when written from a routine';

# 5-6: a typed outer and a differently typed caller shadow: the OUTER rules.
{
    my Str $typed = 'shadow';
    lives-ok { set-typed(5) }, "an Int store to the Int outer passes despite a Str shadow";
    throws-like { set-typed('s') }, X::TypeCheck::Assignment,
        "a Str store to the Int outer dies despite a Str shadow";
}
is $typed, 5, 'the typed outer took the Int';

# 8: Nil resets a typed outer to ITS type object, not the shadow's.
sub reset-typed() { $typed = Nil; $typed }
{
    my Str $typed = 'shadow';
    is reset-typed().raku, 'Int', 'Nil resets to the outer\'s type object';
}

# 9: a nested routine writing its enclosing routine's typed lexical.
sub outer-routine() {
    my Int $z = 1;
    sub inner-routine() { $z = 'bad' }
    inner-routine();
}
throws-like { outer-routine() }, X::TypeCheck::Assignment,
    "a nested routine's write to a typed enclosing lexical is still checked";

# 10-13: across a module boundary -- `Test.rakumod`'s `_init_io` shape.
{
    my Str $output = '';
    is init-output(), 'IO::Handle', "a module routine writes its own untyped lexical";
    is $output, '', "the caller's same-named typed lexical is untouched";
}
{
    my Str $count = 'x';
    is set-count(5), 5, "a module's typed lexical accepts its own type";
    throws-like { set-count('s') }, X::TypeCheck::Assignment,
        "a module's typed lexical still rejects a wrong type";
}
