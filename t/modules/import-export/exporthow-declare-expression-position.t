use Test;

# #13624 (Red, ValueClass): an EXPORTHOW::DECLARE declarator is a term too:
# `given model C { }`, `my model :: { }`, in a list, with `is rw`.
use lib 't/lib';
use DeclareSetRwHow;

plan 5;

is (given frozen Named { has $.a } { .^name }), 'Named', 'given <declarator> Name { }';

my $anon = my frozen :: { has $.a };
is $anon.new(a => 3).a, 3, 'my <declarator> :: { } as a term';

my @seen;
for my frozen :: { has $.a }, my frozen :: { has $.a; has $.b } -> \Cls {
    @seen.push: Cls.^attributes.elems;
}
is @seen, [1, 2], 'two anonymous declarations in a for list';

is (frozen :: { has $.q }).new(q => 5).q, 5, 'bare <declarator> :: { } in parens';
is (frozen :: { }).^name.contains('anon'), True, 'it is anonymous';
