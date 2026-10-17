use Test;

# ADR-11276 §9.151: a method called on a junction autothreads over its values,
# except the methods the junction answers itself: `Junction`'s own (`gist`,
# `raku`, `Str`, `sink`, `Bool`, `defined`, `WHICH`) and `Mu`'s about the
# object (`so`, `not`, `isa`, `does`, `can`). `Junction.Str` threads, as
# Rakudo's `self.THREAD(*.Str)` does. One list serves every call path.

plan 18;

my $j = any(1, 'a');
is $j.gist, 'any(1, a)', 'gist renders each value\'s gist';
is any(1, Nil).gist, 'any(1, Nil)', 'gist shows Nil';
is $j.raku, 'any(1, "a")', 'raku renders each value\'s raku';
is $j.perl, 'any(1, "a")', 'perl is raku';
isa-ok $j.Str, Junction, 'Str threads';
is $j.Str.raku, 'any("1", "a")', 'Str is a junction of Strs';
ok ($j.Str eq '1'), 'a threaded Str compares per value';
is-deeply $j.sink, Nil, 'sink is Nil';

my $n = any(1, 2);
nok $n.isa(Int), 'isa asks the junction itself';
nok $n.does(Int), 'does asks the junction itself';
ok $n.so, 'so collapses';
nok $n.not, 'not collapses';
is $n.can('uc').elems, 0, 'can asks the junction itself';
ok $n.defined, 'defined';
isa-ok $n.WHICH, ObjAt, 'WHICH is the junction\'s';

isa-ok $n.Int, Junction, 'Int threads';
is $n.uc.gist, 'any(1, 2)', 'an undeclared method threads';
is $n.elems.gist, 'any(1, 1)', 'elems threads';
