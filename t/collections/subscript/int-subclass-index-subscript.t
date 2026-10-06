use Test;
use nqp;

# Found via IRC::Log::Textual: Array::Sorted::Util's `finds` returns
# `nqp::box_i($pos, NotFound)` where `NotFound is Int` with `defined` False;
# subscripting by it must still address the slot, as must `1 but R`.
plan 6;

my class NF is Int { method defined(--> False) { } }
my class NG is Int { }
my @a = <x y z>;
my $b := IterationBuffer.CREATE;
$b.push($_) for <a b c>;

my $nf := nqp::box_i(1, NF);
is @a[$nf], "y", 'Array subscript by an is-Int subclass with defined False';
is $b[$nf], "b", 'IterationBuffer subscript likewise';
is @a[nqp::box_i(2, NG)], "z", 'plain is-Int subclass instance as index';
is @a.AT-POS($nf), "y", 'AT-POS agrees';

my $m = 1 but role { };
is @a[$m], "y", 'Int mixin as index';
is $b[$m], "b", 'Int mixin as IterationBuffer index';

done-testing;
