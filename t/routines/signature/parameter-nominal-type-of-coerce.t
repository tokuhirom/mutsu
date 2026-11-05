use Test;

# #15976: `Parameter.nominal_type`, `.of`, `.coerce_type`, `.untyped`, `.onearg`
# and `.is-item`. The expected values are rakudo's.

plan 24;

sub f(Int $x, Mu $m, Int(Str) $c, Str :$s, +@o) {}
my ($x, $m, $c, $s, $o) = &f.signature.params;

is $x.nominal_type.raku, 'Int', 'nominal_type of a plain type';
is $x.of.raku, 'Int', 'of of a plain type';
is $x.coerce_type.raku, 'Mu', 'coerce_type of a plain type is Mu';
is $x.untyped, False, 'a typed parameter is not untyped';

is $m.untyped, True, 'a Mu parameter is untyped';
is $m.nominal_type.raku, 'Mu', 'nominal_type of Mu';

is $c.nominal_type.raku, 'Int', 'a coercion type: the nominal type is the target';
is $c.coerce_type.raku, 'Int', 'a coercion type: coerce_type is the target';
is $c.of.raku, 'Int(Str)', 'a coercion type: of keeps the source';

is $s.nominal_type.raku, 'Str', 'a named typed parameter';
is $s.coerce_type.raku, 'Mu', 'and it is no coercion';

is $o.onearg, True, '+@o is onearg';
is $o.nominal_type.raku, 'Positional', 'a slurpy array is Positional';
is $o.coerce_type.raku, 'Mu', 'and no coercion';
is $x.onearg, False, 'a plain parameter is not onearg';
is $x.is-item, False, 'no parameter is itemized here';

my @methods = <nominal_type of coerce_type untyped onearg is-item>;
for @methods -> $name {
    is $x.can($name).elems, 1, ".can reports $name";
}

sub g(\raw, $u) {}
my @params = &g.signature.params;
is @params[0].nominal_type.raku, 'Any', 'a sigilless parameter is Any';
is @params[1].untyped, False, 'an undeclared type is not untyped';
