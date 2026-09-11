use lib 't/lib';
use Test;

# #7914: an enum key is a package symbol / term, not a `$`-scalar. mutsu stores a
# scalar `$s` sigil-stripped under the env key `s`, so an enum key installed under
# its own bare name shared one namespace with it, and the two clobbered each other
# in both directions. `CSS::Grammar::Defs` declares exactly this shape
# (`:ms<time> :s<time> :px<length>`), so any script that called into CSS::Grammar
# and also used a lexical `$s` had that lexical replaced mid-call.

plan 12;

# --- direction 1: the enum key must not touch a same-named lexical -----------

{
    my $s = 'hello';
    our Str enum InlineUnits « :ms<time> :s<time> »;
    is $s, 'hello', 'an inline enum key leaves a same-named lexical alone';
    is InlineUnits::s.Str, 'time', 'and the enum value is still reachable';
}

{
    # The reported shape: the enum arrives through a module, is imported into a
    # second module's scope, and the consumer merely `use`s that second module.
    use EnumKeyScalarUser;
    my $s = 'hello';
    is $s, 'hello', 'a transitively imported enum key leaves `my $s` alone';
    # Two reads: the clobber happened on a frame reconcile *after* the first one,
    # so a single read could not see it.
    is "[$s]", '[hello]', 'and it is still intact on the next read';
    my $r = EnumKeyScalarUser.unit-of-s;
    is $r.Str, 'time', 'the enum value reached through the module is right';
    is "[$s]", '[hello]', 'a call that returns the enum value does not clobber $s';
}

{
    use EnumKeyScalarDefs :ColliderUnits;
    my $s = 'direct';
    my $px = 'also direct';
    is "$s $px", 'direct also direct',
        'a direct tagged import leaves both colliding lexicals alone';
    is ColliderUnits::px.Str, 'length', 'imported enum value still resolves qualified';
}

# --- direction 2: the lexical must not shadow the enum key -------------------

{
    our Str enum ShadowUnits « :sec<time> »;
    my $sec = 'scalar';
    is sec.Str, 'time', 'a bare enum key reads the enum, not the same-named scalar';
    is $sec, 'scalar', 'and the scalar still reads as itself';
    $sec = 'reassigned';
    is sec.Str, 'time', 'reassigning the scalar does not overwrite the enum key';
}

# --- the enum key is still immutable ----------------------------------------

{
    our enum RoUnits <Alpha>;
    dies-ok { EVAL 'Alpha = 3' }, 'assigning to an enum key is still X::Assignment::RO';
}
