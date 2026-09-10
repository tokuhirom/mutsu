use Test;

plan 10;

# A POSITIONAL postcircumfix index (`[idx]`) on a runtime `Any` value returns
# Any (Rakudo S09), rather than being mis-read as a type parameterization.
# (Regression: `my %h; %h<k>[0]` threw "Any cannot be parameterized".)

my %h;
is-deeply %h<missing>[0], Any, 'positional [0] on a missing hash value is Any';
is-deeply %h<missing>[5], Any, 'positional [5] on a missing hash value is Any';
is-deeply %h<a><b>[0],   Any, 'chained missing-key then [0] is Any';

my @a;
is-deeply @a[99][0], Any, 'nested [i][0] on a missing array slot is Any';

my $any = Any;
is-deeply $any[0], Any, 'positional [0] on a bare Any scalar is Any';

# A slice yields a matching-length list of Any.
is-deeply %h<missing>[0, 1].list, (Any, Any).list, 'slice on Any is a list of Any';
is %h<missing>[0, 1, 2].elems, 3, 'slice length is preserved';

# Non-positional subscript on Any still returns Any (unchanged).
is-deeply %h<missing><b>, Any, 'associative subscript on Any is Any';

# A genuine type parameterization on a non-parametric type still throws.
dies-ok { my $x = Any.WHAT[Int]; }, 'Any[Int] (type parameterization) still dies';

# Real containers still index normally (no accidental capture).
my @real = 10, 20, 30;
is @real[1], 20, 'a real array still indexes normally';
