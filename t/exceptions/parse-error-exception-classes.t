use Test;

# Compile-time diagnoses that mutsu used to collapse into a generic
# X::Syntax::Confused / X::AdHoc. Every expectation here was re-derived from
# `raku` itself, so this file must pass under rakudo unchanged.

plan 20;

my @arr = <1 2 3 4 5>;
my $scalar = 'abcd';
my $array = [1, 2, 3];

sub thrown($code) {
    (try { EVAL $code }, $!).tail;
}

# A `[...]` in infix position is the reduce metaoperator, so its content has to
# be an infix operator.
is thrown(Q[@arr [0]]).^name, 'X::Syntax::Missing',
    'a bracket group in infix position wants an infix inside it';
is thrown(Q[@arr [0]]).what, 'infix inside []', '... and says which one';

# `<digit>.` never starts a fraction here. The sorrow is thrown ALONE when the
# leftover `.` still forms a valid postfix, and inside an X::Comp::Group when
# the retry panics too.
is thrown(Q[42.:all]).^name, 'X::Syntax::Number::IllegalDecimal',
    '42.:all is a lone illegal-decimal sorrow';
is thrown(Q[say 42.:all]).^name, 'X::Syntax::Number::IllegalDecimal',
    'say 42.:all likewise';
is thrown(Q[42. i]).^name, 'X::Syntax::Number::IllegalDecimal',
    'whitespace then a method name recovers, so the sorrow stands alone';
is thrown(Q[42.]).^name, 'X::Comp::Group',
    'a dead-end dot panics as well and groups';
is thrown(Q[42.:]).^name, 'X::Comp::Group', '... as does a dead-end .:';

# Perl 5 dereference blocks, inside and outside an interpolating string.
is thrown(Q[${$scalar}]).^name, 'X::Obsolete', 'bare ${$scalar} is obsolete';
is thrown(Q["${$scalar}"]).^name, 'X::Obsolete', 'and so is the interpolated form';
is thrown(Q[@{$array}]).^name, 'X::Obsolete', 'bare @{$array} is obsolete';
is thrown(Q["@{$array}"]).^name, 'X::Obsolete', 'and so is the interpolated form';
is thrown(Q[${$scalar}]).old, Q[${$scalar}], '.old names the construct as written';

# An infix where a term was expected.
is thrown(Q[(1, , 3)]).^name, 'X::Syntax::InfixInTermPosition',
    'an empty list slot is an infix in term position';
is thrown(Q[my @a = 1, , 2]).^name, 'X::Syntax::InfixInTermPosition',
    '... in an assignment too';

# A placeholder that redeclares a `my` in the same block.
is thrown(Q[ {my $foo; $^foo;}(1) ]).^name, 'X::Redeclaration',
    'my $foo then $^foo is a redeclaration';

# A `*`-curry wrapped in braces is already a closure.
is thrown(Q[{*.abs}]).^name, 'X::Syntax::Malformed', 'a double closure is malformed';
is thrown(Q[{*.{}}()]).^name, 'X::Syntax::Malformed', '... including the zen-slice curry';

# The zen slice the double-closure case is built on, in its dotted spelling.
my %h = a => 1;
is-deeply %h.{}, %h, 'the dotted zen slice selects the whole hash';
is-deeply (*.{})(%h), %h, 'and it curries into a WhateverCode';

# An unterminated regex with a non-ASCII delimiter cannot be anything else.
is thrown("'RT' ~~ m\c[SNOWMAN].\c[COMET]").^name, 'X::Comp::Group',
    'an unterminated unicode-delimited regex is a compile-time group';

# vim: expandtab shiftwidth=4
