use Test;

# A routine called from a lazily pulled `gather` body must see the file-scope
# `our` variables it names (Math::Sequences' `@A002904` reaches Math::Util::Roman's
# `from_roman`, whose nested sub reads the module's `our $rom` / `our @val`).

plan 12;

our $s = 5;
our @arr = 1, 2, 3;
our %h = a => 7;

sub read-scalar { $s }
sub read-array  { @arr.elems }
sub read-hash   { %h<a> }
sub bump        { $s++; $s }
sub nested      { sub inner($c) { $s ~ $c }; inner('!') }

is (gather { take read-scalar() })[0], 5, 'scalar read inside gather';
is (gather { take read-array()  })[0], 3, 'array read inside gather';
is (gather { take read-hash()   })[0], 7, 'hash read inside gather';
is (lazy gather { take read-scalar() })[0], 5, 'scalar read inside lazy gather';
is (gather { take nested() })[0], '5!', 'nested sub read inside gather';

is (gather { take bump() })[0], 6, 'increment inside gather';
is $s, 6, 'increment inside gather reaches the declaring scope';
is (gather { take bump() })[0], 7, 'increment inside gather again';
is $s, 7, 'and the declaring scope sees it';

{
    # a nearer lexical of the same name wins inside the gather body itself
    my $s = 'inner';
    is (gather { take $s })[0], 'inner', 'a captured lexical shadows the file-scope our';
}

my @seen = gather for 1..3 -> $i { take read-scalar() + $i };
is-deeply @seen, [8, 9, 10], 'for loop inside gather';

my @arr2 = gather { @arr.push(4); take read-array() };
is-deeply @arr2, [4], 'array mutation inside gather is seen by the caller of a routine';
