use Test;

# Shell::DSL: `.echo('!!') |> .cat |>> $file.IO;` as a statement. The value of a
# user-declared infix is a fresh object whose `sink` method runs the pipeline;
# mutsu only sank method calls and `but`/`does`, so the statement was a no-op.

plan 4;

my @sunk;
class P {
    has $.n = 0;
    method sink { @sunk.push: $!n }
}
sub mk($n = 0) { P.new(:$n) }
sub infix:«|>>»(*@o) is assoc<list> { P.new(n => 9) }
sub infix:«|>»(*@o) is assoc<list> { P.new(n => 8) }
sub infix:<mine>($a, $b) { P.new(n => 7) }

mk(1);
is-deeply @sunk, [1], 'a plain call is sunk';

@sunk = ();
mk(2) |>> mk(3);
is-deeply @sunk, [9], 'a statement with a declared symbolic infix runs the result sink';

@sunk = ();
mk(4) |> mk(5);
is-deeply @sunk, [8], 'a list-associative one too';

@sunk = ();
1 mine 2;
is-deeply @sunk, [7], 'and a declared word infix';
