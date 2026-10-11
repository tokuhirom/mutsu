use Test;
use NativeCall;

# A CArray[T] is Positional: Any's iteration methods and a non-itemized `for`
# walk its elements instead of treating the array as one item (#10310).
plan 8;

my $a = CArray[uint16].new(1, 2, 3);
is-deeply $a.grep(* > 1).list, (2, 3), 'grep';
is-deeply $a.map(* + 1).list, (2, 3, 4), 'map';
is $a.first(* > 1), 2, 'first';
is $a.tail, 3, 'tail';

class P is repr('CStruct') {
    has Str $.name;
    has CArray[uint16] $.kids;
    submethod BUILD() {}
    submethod TWEAK(:$k, Str:D :$name!) {
        $!name := $name;
        $!kids := CArray[uint16].new(|$k) with $k;
    }
}
my $ps = CArray[P].new(P.new(:name<a>), P.new(:name<b>, :k([1, 2])));
is-deeply $ps.grep(*.kids)».name.list, ('b',), 'grep over CStruct elements';

sub arr { $a }
my @seen;
for arr() -> UInt $n { @seen.push: $n }
is-deeply @seen, [1, 2, 3], 'for over a non-itemized CArray iterates its elements';
@seen = ();
for $a { @seen.push: $_.elems }
is-deeply @seen, [3], 'for over an itemized CArray is one item';
is-deeply (for arr() { .Int }).list, (1, 2, 3), 'for as an expression';
