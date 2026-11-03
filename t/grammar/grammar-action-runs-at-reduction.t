use Test;

plan 9;

# A subrule's action runs when the subrule reduces, before the next step of the
# enclosing pattern (#14517, ADR-7576): a `{ ... }` block after `<a>` sees its
# side effects and its `.made`.

my @log;
grammar G {
    token TOP { <a> { @log.push("code") } <b> }
    token a { x }
    token b { y }
}
class A {
    method a($/) { @log.push("a") }
    method b($/) { @log.push("b") }
    method TOP($/) { @log.push("TOP") }
}
G.parse("xy", :actions(A));
is @log.join(","), "a,code,b,TOP", 'action a ran before the code block that follows <a>';

my $seen;
grammar H {
    token TOP { <n> { $seen = $<n>.made } <m> }
    token n { \d+ }
    token m { \w+ }
}
class HA { method n($/) { make +$/ * 2 } }
H.parse("21ab", :actions(HA));
is $seen, 42, '$<n>.made is already set inside the code block after <n>';

# Outer lexicals an action writes are visible once the parse is done.
my $count = 0;
grammar I { token TOP { <w>+ % ' ' }; token w { \w+ } }
class IA { method w($/) { $count++ } }
I.parse("a bb ccc", :actions(IA));
is $count, 3, 'an action rebinding an outer lexical reaches the caller';

# Attributes of the actions object survive the mid-match dispatch.
class Counter {
    has @.words;
    method w($/) { @!words.push(~$/) }
}
my $c = Counter.new;
I.parse("a bb ccc", :actions($c));
is $c.words.join(","), "a,bb,ccc", 'actions object state accumulates across reductions';

# A reduction abandoned by backtracking keeps its effects, in order.
my @order;
grammar J {
    token TOP { <x> <y> || <x> <z> }
    token x { a }
    token y { b }
    token z { c }
}
class JA {
    method x($/) { @order.push("x") }
    method y($/) { @order.push("y") }
    method z($/) { @order.push("z") }
    method TOP($/) { @order.push("TOP") }
}
J.parse("ac", :actions(JA));
is @order.join(","), "x,x,z,TOP", 'the abandoned <x> reduction ran before the retry';

# A failed parse leaves the effects of the reductions that happened.
my @failed;
grammar K { token TOP { <p> <q> }; token p { a }; token q { b } }
class KA { method p($/) { @failed.push("p") } }
K.parse("ax", :actions(KA));
is @failed.join(","), "p", 'a failed parse still ran the matched subrule action';

# An aliased subrule dispatches once.
my $alias_runs = 0;
grammar L { token TOP { <v=num> }; token num { \d+ } }
class LA { method num($/) { $alias_runs++ } }
L.parse("7", :actions(LA));
is $alias_runs, 1, 'a non-suppressing alias fires the action once';

# A `die` in an action propagates out of the parse.
grammar M { token TOP { <a> <b> }; token a { x }; token b { y } }
class MA { method a($/) { die "boom" } }
throws-like { M.parse("xy", :actions(MA)) }, Exception, message => /boom/,
    'an exception from a reduction-time action leaves the parse';

# `make` in the start rule still gives the parse its value.
class NA { method TOP($/) { make "top:" ~ $<a>.made }; method a($/) { make "a" } }
grammar N { token TOP { <a> }; token a { x } }
is N.parse("x", :actions(NA)).made, "top:a", 'children made values reach the start rule action';
