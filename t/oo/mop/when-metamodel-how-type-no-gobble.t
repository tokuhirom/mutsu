use v6;
use Test;

# `when Metamodel::ClassHOW { ... }` (and its `Metamodel::*HOW` siblings) must
# smartmatch against the real Metamodel type, not gobble the block as a call
# argument to an undeclared routine named `Metamodel::ClassHOW`. These types
# are real and already smartmatch-able at runtime (`Foo.HOW ~~
# Metamodel::ClassHOW` works), but the parser's `when`-gobbled-block guard
# consulted a separate "known compound type" list that had never heard of the
# `Metamodel::` family, so it treated the bareword as an undeclared routine
# call and reported "needs parens to avoid gobbling block" instead.
#
# Regression: ecosystem/dists/A/AttrX--Mooish's `is mooish` trait handler does
# exactly `given $*PACKAGE.HOW { when Metamodel::ClassHOW { ... } }`.

plan 6;

class Foo { }

my $branch = 'none';
given Foo.HOW {
    when Metamodel::ClassHOW { $branch = 'class' }
    when Metamodel::GrammarHOW { $branch = 'grammar' }
    default { $branch = 'default' }
}
is $branch, 'class', 'when Metamodel::ClassHOW matches a plain class HOW';

$branch = 'none';
given Foo.HOW {
    when Metamodel::GrammarHOW { $branch = 'grammar' }
    default { $branch = 'default' }
}
is $branch, 'default', 'when Metamodel::GrammarHOW does not match a class HOW';

# The fully-qualified `Perl6::Metamodel::ClassHOW` spelling is NOT
# pre-declared for this check in rakudo either (verified: it gobbles the
# block there too), so this stays scoped to the bare `Metamodel::*` spelling
# rather than diverging from the reference by accepting more than rakudo does.
throws-like
    'class QuxHOWTest { }; given QuxHOWTest.HOW { when Perl6::Metamodel::ClassHOW { 1 }; default { 1 } }',
    X::Comp::Group,
    "when Perl6::Metamodel::ClassHOW { } still gobbles the block, matching rakudo";

# The rest of the HOW family the parser now also recognizes.
for <Metamodel::ParametricRoleHOW Metamodel::EnumHOW Metamodel::ModuleHOW> -> $type-name {
    my $ok = try {
        EVAL "given Foo.HOW \{ when $type-name \{ 1 \} ; default \{ 1 \} \}";
        True
    };
    ok $ok, "when $type-name \{ \} parses without gobbling the block";
}
