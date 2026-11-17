use Test;

plan 10;

my @seen;
my $mapped = (^5).map({ @seen.push($_); $_ * 10 });
is $mapped.first(* == 20), 20, 'first finds the mapped value';
is-deeply @seen, [0, 1, 2], 'a named Seq stops mapping at the first match';

@seen = ();
$mapped = (^5).map({ @seen.push($_); $_ * 10 });
is $mapped.first(* == 20, :k), 2, ':k returns the source index';
is-deeply @seen, [0, 1, 2], ':k stops mapping at the match';

@seen = ();
$mapped = (^5).map({ @seen.push($_); $_ * 10 });
is $mapped.first(* == 20, :kv).raku, '(2, 20)', ':kv returns index and value';
is-deeply @seen, [0, 1, 2], ':kv stops mapping at the match';

@seen = ();
$mapped = (^5).map({ @seen.push($_); $_ * 10 });
is $mapped.first(* == 20, :p).raku, '2 => 20', ':p returns an index/value pair';
is-deeply @seen, [0, 1, 2], ':p stops mapping at the match';

@seen = ();
$mapped = (^5).map({ @seen.push($_); $_ * 10 });
is $mapped.first(* == 99), Nil, 'a missing value returns Nil';
is-deeply @seen, [0, 1, 2, 3, 4], 'a missing value reads the whole Seq';
