use v6;
use Test;
use lib 't/lib';
use EvalEnumKeyGlobalFixture;
use EvalEnumKeyPrivateFixture;

# A bare enum key (`aa`, not `E::aa`) is a declared term for an EVAL'd snippet
# in the scope that declared the enum (#11818). Enum keys live in their own
# bare-name namespace rather than under the plain env key, which is where the
# EVAL undeclared-name / undeclared-routine checks used to look.
#
# Every expectation below was verified against Rakudo.

plan 10;

enum E <aa bb>;

is EVAL("aa"), E::aa, 'an enum key declared in the calling scope resolves in EVAL';
is EVAL("bb"), E::bb, 'a second key of the same enum resolves';
is EVAL("E::aa"), E::aa, 'the qualified spelling still works';

sub in-sub { EVAL "bb" }
is in-sub(), E::bb, 'the key resolves in an EVAL inside a sub';

{
    my $x = 3;
    enum Inner <ii jj>;
    is EVAL("jj + $x"), 4, 'a key declared in a nested block resolves in that block';
}

# A key from a package-less module file is declared in GLOBAL.
is EVAL("EvalFixtureSA").value, 1, "a loaded module's package-less enum key resolves in EVAL";
sub in-sub-module { EVAL "EvalFixtureSB" }
is in-sub-module().value, 2, "...also from an EVAL inside a sub";

# A `unit module`'s own enum key stays private to that module.
is peek-key().value, 7, "the module's own code still sees its private enum key";
throws-like { EVAL "EvalFixturePK" }, X::Undeclared::Symbols,
    "a unit module's private enum key stays undeclared in the importer's EVAL";

# An unrelated bareword is still undeclared.
throws-like { EVAL "no-such-key-here" }, X::Undeclared::Symbols,
    'an unknown bareword is still undeclared in EVAL';
