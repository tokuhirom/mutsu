use v6;
use Test;

# ADR-11678 §8 / #13026: a plain `gather` passed to a builtin routine is read
# the way Rakudo's routine reads it. Streaming routines (`first`, `zip`, `map`)
# pull an infinite gather only as far as they need and consume it; iterating
# ones (`sum`, `max`, `unique`, `classify`) consume it; the rest (`elems`,
# `join`, `sort`, `set`, string coercions) read it through its cache and leave
# it readable. Every expectation is Rakudo v2026.09's output.

plan 34;

sub g() { gather { take 5; take 2 } }
sub inf() { gather { my $i = 0; loop { take $i++ } } }

# Whether `&read` consumed the gather it was handed.
sub consumes(&read) {
    my \s = g();
    read(s);
    (try s.List) === Nil
}

# --- The elements reach the builtin -------------------------------------------

is sum(g()), 7, 'sum';
is min(g()), 2, 'min';
is max(g()), 5, 'max';
is minmax(g()), 2..5, 'minmax';
is unique(g()).List, (5, 2), 'unique';
is classify({ $_ > 3 }, g()).keys.sort.List, (False, True), 'classify';
is reduce(&[+], g()), 7, 'reduce';
is kv(g()).List, (0, 5, 1, 2), 'kv';
is cross(g(), <a b>).elems, 4, 'cross';
is join(",", g()), '5,2', 'join';
is sort(g()).List, (2, 5), 'sort';
is set(g()).elems, 2, 'set';
is uc(g()), '5 2', 'uc reads the Str of the whole gather';
is chrs(g()), "\x[5]\x[2]", 'chrs';

# --- Consumption, as Rakudo's routine has it ----------------------------------

ok consumes({ sum($_) }), 'sum consumes the gather';
ok consumes({ max($_) }), 'max consumes the gather';
ok consumes({ unique($_) }), 'unique consumes the gather';
ok consumes({ first(* > 3, $_) }), 'first consumes the gather';
ok consumes({ zip($_, <a b>) }), 'zip consumes the gather';
ok consumes({ map(* + 1, $_) }), 'map consumes the gather';
ok consumes({ cross($_, <a>) }), 'cross consumes the gather';
nok consumes({ elems($_) }), 'elems leaves the gather readable';
nok consumes({ join(",", $_) }), 'join leaves the gather readable';
nok consumes({ sort($_) }), 'sort leaves the gather readable';
nok consumes({ set($_) }), 'set leaves the gather readable';
nok consumes({ uc($_) }), 'uc leaves the gather readable';

{
    my \s = g();
    sum(s);
    throws-like { sum(s) }, X::Seq::Consumed, 'a second sum of the same gather throws';
}

# --- Streaming routines stay lazy over an infinite gather ----------------------

is zip(inf(), <a b>).List, ((0, 'a'), (1, 'b')), 'zip over an infinite gather';
is roundrobin(<a b>, inf()).head(4).List, (('a', 0), ('b', 1), (2,), (3,)),
    'roundrobin over an infinite gather';
is map(* * 2, inf()).head(3).List, (0, 2, 4), 'map over an infinite gather';
is grep(* %% 2, inf()).head(3).List, (0, 2, 4), 'grep over an infinite gather';
is list(inf()).head(3).List, (0, 1, 2), 'list hands the gather back';

# --- A cached gather is served from its cache ----------------------------------

{
    my \s = g();
    s[0];
    is sum(s), 7, 'sum of a cached gather';
    is s.List, (5, 2), 'a cached gather stays readable after sum';
}
