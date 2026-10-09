use v6;
use Test;

# Found via Data::Transformers (t/04-accumulate.rakutest): `given @d` binds
# `$_` to the Array itself (no Scalar), so `for $_` iterates its elements;
# an itemized topic (`given $x`, `for $x { for $_ }`) stays one item.

plan 10;

my @d = 3, 4;
my @seen;

given @d { for $_ { @seen.push($_) } }
is-deeply @seen, [3, 4], 'for $_ under given @d iterates elements';

@seen = ();
given @d { for $_ -> $a { @seen.push($a) } }
is-deeply @seen, [3, 4], 'for $_ -> $a under given @d iterates elements';

@seen = ();
given @d { for $_ -> $a, $b { @seen.push($a + $b) } }
is-deeply @seen, [7], 'two-parameter pointy block chunks the topic array';

my $x = [1, 2];
@seen = ();
given $x { for $_ -> $a { @seen.push($a) } }
is @seen.elems, 1, 'for $_ under given $x is one itemized element';

@seen = ();
for $x { for $_ -> $a { @seen.push($a) } }
is @seen.elems, 1, 'nested for over itemized topic is one element';

my @rows = [10, 7], [2, 8];
sub rows(@data) {
    my @res;
    given @data {
        when True {
            for $_ -> @r { @res.push(@r.Array) }
            return @res.Array;
        }
    }
}
is-deeply rows(@rows), [[10, 7], [2, 8]], 'return from for-given-when keeps rows';

@seen = ();
for @rows { for $_ { @seen.push($_) } }
is @seen.elems, 2, 'array elements of @rows are itemized topics';

@seen = ();
for 5 { for $_ { @seen.push($_) } }
is-deeply @seen, [5], 'plain scalar topic iterates once';

@seen = ();
with @d { for $_ { @seen.push($_) } }
is-deeply @seen, [3, 4], 'for $_ under with @d iterates elements';

@seen = ();
for (1, 2), (3, 4) { for $_ { @seen.push($_) } }
is-deeply @seen, [1, 2, 3, 4], 'plain List topics flatten';
