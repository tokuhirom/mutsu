use Test;

# `.base`'s radix is bound as each owner's signature binds it (#14037):
# `Int.base(Int(Cool) $base)` coerces, `Rational.base(Int:D $base)` has no
# candidate for anything else, and `Num`/`Real`'s `Int:D $base` parameter
# refuses it.

plan 16;

is 10.base("2"), '1010', 'Int.base numifies a Str radix';
is 10.base(2.5), '1010', 'Int.base truncates a fractional radix';
is 10.base(2.9, *), '1010', 'Int.base(Rat, *) truncates too';
throws-like { 10.base("x") }, X::Str::Numeric, 'Int.base with a non-numeric Str radix';
is 10.base(<2>), '1010', 'an IntStr radix is an Int';

throws-like { 3.5.base("2") }, X::Multi::NoMatch,
    message => /'Cannot resolve caller base(Rat:D: Str:D)'/, 'Rat.base refuses a Str radix';
throws-like { 3.5.base(2.0) }, X::Multi::NoMatch,
    message => /'Cannot resolve caller base(Rat:D: Rat:D)'/, 'Rat.base refuses a Rat radix';
throws-like { 10.FatRat.base("2") }, X::Multi::NoMatch,
    message => /'Cannot resolve caller base(FatRat:D: Str:D)'/, 'FatRat.base refuses a Str radix';
is 3.5.base(<2>), '11.1', 'Rat.base takes an IntStr radix';
is 3.5.base(2), '11.1', 'Rat.base takes an Int radix';

throws-like { 2.5e0.base("2") }, X::TypeCheck::Binding::Parameter,
    message => q[Type check failed in binding to parameter '$base'; expected Int but got Str ("2")],
    'Num.base refuses a Str radix';
throws-like { 2.5e0.base(2.0) }, X::TypeCheck::Binding::Parameter, 'Num.base refuses a Rat radix';
throws-like { Duration.new(3).base("2") }, X::TypeCheck::Binding::Parameter,
    'Duration.base refuses a Str radix';
is Duration.new(100).base(2), '1100100', 'Duration.base takes an Int radix';

is-deeply 3.5.polymod("2"), (1.5, 1.0).Seq, 'Rat.polymod numifies a Str divisor to its Int';
throws-like { 3.5.polymod("x").eager }, X::Str::Numeric, 'Rat.polymod with a non-numeric Str divisor';
