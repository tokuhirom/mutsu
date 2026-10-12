use Test;

# `Any.pick`, `Any.roll` and `Any.minmax` are `self.list.METHOD` in Rakudo
# (ADR-11276 §9.83): every receiver without its own candidate reads as its
# list, whether it has a dispatch shape (Capture, Blob, IO::Path, Version)
# or not (a Seq, a Slip, a code object, an instance, a type object).
# `Mu` is no `Any`, so it has no candidate at all.

plan 24;

class P { }
class C is Cool { method Str { "c" } }

my $p = P.new;
ok $p.pick === $p, 'an instance picks itself';
is $p.roll(3).elems, 3, 'an instance rolls three times';
ok C.new.pick ~~ C, 'a Cool instance picks itself';
is (1, 2, 3).map(* * 2).pick(*).sort.join(','), '2,4,6', 'a Seq picks from its elements';
ok (gather { take 7 }).roll ~~ 7, 'a gather Seq rolls';
ok slip(4, 5).pick ~~ 4..5, 'a Slip picks';
ok \(1, 2, :a).pick ~~ 1..2, 'a Capture picks from its positionals';
ok Blob.new(1, 2, 3).roll ~~ 1..3, 'a Blob rolls a byte';
ok "foo".IO.pick ~~ IO::Path, 'an IO::Path picks itself';
is Version.new('1.2.3').roll(2).elems, 2, 'a Version rolls itself';
my &s = sub { 42 };
ok &s.pick === &s, 'a Sub picks itself';
ok /a/.roll ~~ Regex, 'a Regex rolls itself';
ok Int.pick === Int, 'a type object picks itself';
ok Int.roll === Int, 'a type object rolls itself';
is pick(2, <z z z>), <z z>, 'the sub form of pick';
is roll(3, <z>).elems, 3, 'the sub form of roll';

throws-like { Mu.pick }, X::Method::NotFound, 'Mu.pick has no candidate';
throws-like { Mu.roll(2) }, X::Method::NotFound, 'Mu.roll has no candidate';
throws-like { Mu.flat }, X::Method::NotFound, 'Mu.flat has no candidate';

is (3, 1, 2).map(* + 0).minmax, 1..3, 'a Seq minmax';
is \(5, 2, 9).minmax, 2..9, 'a Capture minmax';
is-deeply (1..^5).minmax, (1, 4), 'an Int Range minmax counts its excluded end';
is (2, 8).minmax, 2..8, 'a List minmax';
ok $p.minmax ~~ Range, 'an instance minmax is its one-element range';
