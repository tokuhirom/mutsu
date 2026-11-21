use Test;

# #18259: a lazily consumed `gather do { ... }` whose block declares a routine
# suspends at each `take` and resumes inside the block, so the rest of the
# block runs and every call of the enclosing routine takes its values.

plan 9;

sub f($n) { gather do { .take for i($n); sub i($m) { ($m,) } } }
sub p($n) { f($n).map({ $_ }) }
is-deeply p(3), (3,).Seq, 'first call takes its value';
is-deeply p(6), (6,).Seq, 'second call takes its value too';

sub two($n) { gather do { take i($n); take i($n + 1); sub i($m) { $m * 10 } } }
is-deeply two(3).map({ $_ }).List, (30, 40), 'a take after the resume point still calls the inner sub';
is-deeply two(6).head(1).List, (60,), 'a partial pull';
is-deeply two(7).map({ $_ }).List, (70, 80), 'and a full pull after it';

sub looped($n) { gather do { for ^2 -> $k { take j($k + $n) }; take j(0); sub j($m) { $m + 100 } } }
is-deeply looped(1).map({ $_ }).List, (101, 102, 100), 'a take inside a loop in the block';

my @seen;
sub noted($n) { gather do { @seen.push("body $n"); take $n; @seen.push("after $n"); sub z { } } }
noted(1).map({ $_ }).eager;
noted(2).map({ $_ }).eager;
is-deeply @seen.List, ('body 1', 'after 1', 'body 2', 'after 2'), 'the block runs to its end';

sub outer-name { 'outer' }
is-deeply (gather do { take outer-name(); take outer-name(); sub outer-name { 'inner' } }).map({ $_ }).List,
    ('inner', 'inner'), 'the inner sub shadows an outer one across the resume';
is outer-name(), 'outer', 'and does not leak out of the block';
