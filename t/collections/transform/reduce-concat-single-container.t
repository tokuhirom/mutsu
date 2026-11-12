use Test;

# `[~] ($x,)` where `$x` is a lexical captured by a closure: the lone operand
# reaches the reduction as a reference to the variable's shared container, and
# the `infix:<~>` candidate is still chosen by the value it holds (found running
# RSV, whose `to-rsv` folds `[~] (|@values.map(...), EOR)` over a module
# constant Blob inside a pointy block).

plan 4;

sub blob-closure() { my $x = blob8.new(253); -> { [~] ($x,) } }
is blob-closure()().raku, 'Blob[uint8].new(253)', 'a captured Blob stays a Blob';

sub list-closure() { my $x = (1, 2); -> { [~] ($x,) } }
is list-closure()(), '12', 'a captured List is joined';

sub int-closure() { my $x = 5; -> { [~] ($x,) } }
is int-closure()().WHAT.^name, 'Str', 'a captured scalar is stringified';
is int-closure()(), '5', 'with its value';
