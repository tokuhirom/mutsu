use Test;

# A class's own `method ^parameterize` runs when the code spelling `C[T]` is
# compiled (#17085): before the unit's run-time statements, in source order
# with its BEGIN blocks, and not again when the statement runs (a loop body's
# `my C[T] $y`). Checked against rakudo, which calls it once per spelling in
# the source; mutsu builds one type per argument types, so only the order and
# the absence of run-time calls are pinned.

plan 9;

my @log;
my class Foo is Array {
    method ^parameterize(Mu:U \arr, Mu \of) {
        @log.push(of.^name);
        my $what := arr.^mixin(Array::Typed[of]);
        $what.^set_name("{arr.^name}[{of.^name}]");
        $what
    }
}
BEGIN @log.push('begin');
@log.push('run');
my Foo[Int] $x = Foo[Int].new(1, 2);
for ^2 { my Foo[Str] $y; @log.push('loop') }
sub f(Foo[Int] $p) { $p.elems }

is @log[0], 'begin', 'a BEGIN written earlier runs first';
is-deeply @log[1 .. *].grep(* ne 'Int' | 'Str').List, ('run', 'loop', 'loop'),
    'then the parameterizations, before any run-time statement';
ok @log.first('run', :k) > @log.first('Int', :k), 'Foo[Int] was built at compile time';
ok @log.first('run', :k) > @log.first('Str', :k), 'so was the loop body\'s Foo[Str]';
is $x.^name, 'Foo[Int]', 'the variable has the parameterized type';
is-deeply $x.List, (1, 2), 'and holds the values';
is f($x), 2, 'a parameter typed Foo[Int] accepts it';

my $before = +@log;
sub mk(\t) { Foo[t] }
my $t = mk(Rat);
is $t.^name, 'Foo[Rat]', 'a run-time argument parameterizes at run time';
is +@log, $before + 1, 'calling the meta-method then';
