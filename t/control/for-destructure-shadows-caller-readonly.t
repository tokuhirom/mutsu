use v6;
use Test;

# A pointy block's destructuring sub-signature (`-> ($f, $w)`) declares
# fresh lexicals. It used to assign them by name instead, so a callee's
# `-> ($f, $w)` saw the CALLER's readonly `$f` (a `for ... -> $f` alias or a
# routine parameter) and died with "Cannot assign to a readonly variable or
# a value". Found via Prettier::Table, whose `!stringify-hrule` loops
# `-> ($field, $width, $align)` while its test loops `-> $field`.
plan 8;

sub pairs-of() {
    my @out;
    for ((1, 2), (3, 4)) -> ($f, $w) { @out.push("$f$w") }
    @out.join(',')
}

for <x> -> $f {
    is pairs-of(), '12,34', 'callee destructure under a caller for-loop alias';
}

sub with-param($f) { pairs-of() }
is with-param(1), '12,34', 'callee destructure under a caller readonly parameter';

class Row {
    has @.names = <a b c>;
    method zipped() {
        my @bits;
        for (@!names Z @!names) -> ($field, $width) { @bits.push("$field$width") }
        @bits.join(',')
    }
}
for <x y> -> $field {
    is Row.new.zipped, 'aa,bb,cc', "method destructure under caller alias ($field)";
}

# The destructured names are block lexicals: they shadow, never clobber, an
# outer variable of the same name.
my $a = 'outer';
for ((1, 2),) -> ($a, $b) { }
is $a, 'outer', 'destructure target shadows the outer $a';

# An `is raw` hash target still binds the element's container.
my %h is default(42);
for ((%h,),) -> (%x is raw) {
    is %x.VAR.default, 42, 'is raw hash target keeps the container default';
}

# A destructured target of an array still receives the array itself.
for (([1, 2, 3], 'z'),) -> (@xs, $z) {
    is @xs.elems, 3, 'array target binds the nested array';
    is $z, 'z', 'the scalar sibling binds the next element';
}
