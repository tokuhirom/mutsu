use Test;

# `.raku` of an explicitly `.lazy` Seq, finite or not (#10918). Expected
# strings are rakudo's.

plan 14;

is (5,).lazy.raku, '(5).lazy.Seq', 'a one-element .lazy keeps its element, no trailing comma';
is (1, 2).lazy.raku, '(1, 2).lazy.Seq', 'a finite .lazy list renders every element';
is (1..3).lazy.raku, '(1, 2, 3).lazy.Seq', 'a finite .lazy range renders every element';
is ().lazy.raku, '().lazy.Seq', 'an empty .lazy list';
is (5,).lazy.head(3).List.raku, '(5,)', 'a bounded pull of a short .lazy list sees its element';

is (1..5).lazy.map(* + 1).raku, '(2, 3, 4, 5, 6).lazy.Seq', 'a map over a finite .lazy list stays lazy';
is (1..5).lazy.grep(* > 2).raku, '(3, 4, 5).lazy.Seq', 'a grep over a finite .lazy list stays lazy';
ok (1..5).lazy.map(* + 1).is-lazy, 'map over .lazy is .is-lazy';
nok (1..5).map(* + 1).is-lazy, 'map over a plain range is not';

my @seen;
my $mapped = (1..3).lazy.map({ @seen.push($_); $_ * 2 });
is @seen.elems, 0, 'the map callback does not run before the Seq is pulled';
is $mapped.head(2).List, (2, 4), 'pulling runs it on demand';

is (1..*).lazy.^name, 'Seq', '.lazy on an infinite range is a Seq';
ok (1..*).lazy.raku.ends-with(' 99, 100...).lazy.Seq'), 'and renders a 100-element prefix';

my $s = (1, 2, 3).lazy;
is $s.raku, '$((1, 2, 3).lazy.Seq)', 'a $-held lazy Seq renders itemized';
