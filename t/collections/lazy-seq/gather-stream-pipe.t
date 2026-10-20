use Test;

# The streaming consumers of a plain gather read it through a pipe over its
# Seq, never through its inner coroutine list (#13026, ADR-11678): an infinite
# gather streams, an uncached one is consumed, a cached one stays readable,
# and the pipe is exactly as lazy as the gather. Expected values are the
# `raku` (v2026.09) output.

plan 11;

sub inf { gather { my $i = 0; loop { take $i++ } } }

nok (gather { take 1; take 2; take 3 }).map(* + 1).is-lazy, '.map over a gather is not lazy';
is (gather { take 1; take 2; take 3 }).map(* + 1).elems, 3, '... and answers .elems';
is (inf() Z <a b c>).join(','), '0 a,1 b,2 c', 'Z streams an infinite gather';
is ([\+] inf())[^4].join(','), '0,1,3,6', 'a triangle reduction streams an infinite gather';
is inf.first(* > 5), 6, '.first streams an infinite gather';
is inf.grep(* %% 3).head(3).join(','), '0,3,6', '.grep streams an infinite gather';

my $g = gather { take 1; take 2 };
my $m = $g.map(* * 10);
throws-like { $g.elems }, X::Seq::Consumed, '.map consumes an uncached gather';

my $c = gather { take 1; take 2 };
$c.cache;
my $cm = $c.map(* * 10);
is "{$cm.join(',')} {$c.join(',')}", '10,20 1,2', '.map over a cached gather leaves it readable';

ok (1,).map({ lazy gather take 5 })[0].is-lazy, 'a lazy gather returned from a map block stays lazy';
is inf.lazy.head(2).join(','), '0,1', '.lazy streams an infinite gather';
my $it = inf.iterator;
is "{$it.pull-one}{$it.pull-one}", '01', '.iterator resumes the gather per pull-one';
