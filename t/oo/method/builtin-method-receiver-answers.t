use Test;

# The answers the VM's native method entry used to reach through by-name
# receiver bypasses (ADR-11276 slice 5, #11276). The rows answer each of these
# receivers; every expected value is Rakudo's.

plan 31;

# A user `Real` class: the `Real` role's methods bridge through `Bridge`.
class R does Real { has $.v; method Bridge { $!v.Num } }
is R.new(v => -2).abs, 2, 'Real.abs bridges';
is R.new(v => 2.5).floor, 2, 'Real.floor bridges';
is R.new(v => 2.5).Int, 2, 'Real.Int bridges';
is R.new(v => 4).sqrt, 2, 'Real.sqrt bridges';
is R.new(v => -4).sign, -1, 'Real.sign bridges';

# A collection holding an instance renders the instance through its own methods.
class P { has $.x; method raku { "P<$!x>" }; method gist { "g$!x" } }
is [P.new(x => 1), 2].raku, '[P<1>, 2]', 'Array.raku dispatches an element raku';
is [P.new(x => 1), 2].gist, '[g1 2]', 'Array.gist dispatches an element gist';
is {a => P.new(x => 1)}.raku, '{:a(P<1>)}', 'Hash.raku dispatches an element raku';
is {a => P.new(x => 1)}.gist, '{a => g1}', 'Hash.gist dispatches an element gist';
is (P.new(x => 1),).gist, '(g1)', 'List.gist dispatches an element gist';

# Typed containers keep their type through `raku` and `clone`.
my Int @ai = 1, 2;
is @ai.raku, 'Array[Int].new(1, 2)', 'typed Array.raku names the type';
ok @ai.clone ~~ Array[Int], 'typed Array.clone keeps the type';
my Int %hi = a => 1;
ok %hi.clone ~~ Hash[Int], 'typed Hash.clone keeps the type';
my %m is Map = a => 1, b => 2;
is %m.gist, 'Map.new((a => 1, b => 2))', 'Map.gist';
is %m.raku, 'Map.new((:a(1),:b(2)))', 'Map.raku';

# `unique`/`repeated`/`squish` compare by a user `WHICH`.
class W { has $.k; method WHICH { ValueObjAt.new("W|$!k") } }
is (W.new(k => 1), W.new(k => 1), W.new(k => 2)).unique.elems, 2, 'unique by user WHICH';
is (W.new(k => 1), W.new(k => 1)).repeated.elems, 1, 'repeated by user WHICH';
is (W.new(k => 1), W.new(k => 1), W.new(k => 2)).squish.elems, 2, 'squish by user WHICH';

# A user class with its own `iterator` is iterated through it.
class I { method iterator { (1, 2, 3).iterator } }
is-deeply I.new.map(* * 2).List, (2, 4, 6), 'map reads a user iterator';
is-deeply I.new.grep(* > 1).List, (2, 3), 'grep reads a user iterator';
is I.new.first(* > 1), 2, 'first reads a user iterator';
is-deeply I.new.sort(-*).List, (3, 2, 1), 'sort reads a user iterator';
is-deeply I.new.head(2).List, (1, 2), 'head reads a user iterator';
is-deeply I.new.tail(2).List, (2, 3), 'tail reads a user iterator';

# A `Seq.new($iterator)` that nothing has pulled yet answers its elements.
class It does Iterator { has $.n = 0; method pull-one { $!n < 3 ?? $!n++ !! IterationEnd } }
is Seq.new(It.new).elems, 3, 'untouched Seq.elems';
is-deeply Seq.new(It.new).List, (0, 1, 2), 'untouched Seq.List';
is Seq.new(It.new).join(','), '0,1,2', 'untouched Seq.join';
is Seq.new(It.new).sum, 3, 'untouched Seq.sum';

# A Supply answers its own methods.
is-deeply Supply.from-list(3, 1, 2).sort.list, (1, 2, 3), 'Supply.sort';
is-deeply Supply.from-list(1, 2, 3).head(2).list, (1, 2), 'Supply.head';

# The core type objects' class-level methods.
use nqp;
is nqp::elems(Thread.usage), 6, 'Thread.usage';
