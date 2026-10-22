use Test;

# #14594 (ValueClass t/01-basic.rakutest): a closure created in a single-param
# `for ... -> \Name` body reads that loop's binding even when a LATER
# multi-param loop in the same scope declares `\Name` too.

plan 4;

sub run(&c) { c() }

my @seen;
for 10, 20 -> \Bla { run({ @seen.push: Bla }) }
for 2, 3 -> \Bla, $x { @seen.push: Bla + $x }
is @seen, [10, 20, 5], 'sigilless loop param read inside a closure';

my @objs;
for class :: { has $.a }, class :: { has $.a; has $.b } -> \Cls {
    run({ @objs.push: Cls.new.defined });
}
for class :: { }, 5 -> \Cls, $n { }
is @objs, [True, True], 'a type object bound to the sigilless param';

my @late;
for 1 -> \Foo { run({ @late.push: Foo }) }
for 4, 5 -> \Qux, $x { @late.push: Qux }
is @late, [1, 4], 'a differently named later loop was never affected';

my @scalar;
for 7 -> $v { run({ @scalar.push: $v }) }
for 8, 9 -> $v, $y { @scalar.push: $v }
is @scalar, [7, 8], 'scalar param (already fine)';
