use Test;

# `split` is a row of `Str`, `Cool`, `IO::Path`, `IO::Handle` and the
# `IO::Spec` classes, found through the receiver's MRO like the other text
# methods (ADR-11276 §9.182): no by-name arm sends every receiver to one routine.

plan 12;

is-deeply (1, 2, 3).split(" "), ("1", "2", "3").Seq, 'a List is Cool';
is-deeply 1234.split("3"), ("12", "4").Seq, 'an Int';
is-deeply (1..3).split(" "), ("1", "2", "3").Seq, 'a Range';
is-deeply "a,b,c".split(",", 2), ("a", "b,c").Seq, 'a limit';
is-deeply "a1b2c".split(/\d/, :v).map(~*), ("a", "1", "b", "2", "c").Seq, 'a regex with :v';
is-deeply "a-b-c".split(<- b>), ("a", "", "", "c").Seq, 'a list of needles';
is-deeply "a-b".split(gather { take "-" }), ("a", "b").Seq, 'a gather of needles';

class C is Cool { method Str { "x-y" } }
is-deeply C.new.split("-"), ("x", "y").Seq, 'a Cool subclass splits its own Str';

enum E <foo-bar>;
is-deeply foo-bar.split("-"), ("foo", "bar").Seq, 'an enum value of a Str enum';

grammar G { token TOP { <w>+ }; token w { \w } }
is-deeply G.parse("ab1").split("b"), ("a", "1").Seq, 'a grammar cursor';

my $path = $*TMPDIR.add("mutsu-split-receivers-{$*PID}.txt");
$path.spurt("p q r");
is-deeply $path.split(" ").List, ("p", "q", "r"), 'IO::Path splits its contents';
is-deeply $path.open.split(" ").List, ("p", "q", "r"), 'and so does an IO::Handle';
$path.unlink;
