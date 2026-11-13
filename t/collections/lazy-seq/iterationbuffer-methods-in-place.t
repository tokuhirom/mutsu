use Test;

plan 15;

# IterationBuffer's methods edit the buffer in place (#17337): a List view
# taken earlier sees later edits, as the buffer is that List's storage.

sub ib(*@v) { my $x := IterationBuffer.new; $x.push($_) for @v; $x }

my $b := IterationBuffer.new;
$b.push(1);
$b.push(2);
is $b.elems, 2, 'push grows the buffer';
my $view := $b.List;

$b.append(ib(3, 4));
is $b.elems, 4, 'append adds every element';
is $view.elems, 4, 'a List taken earlier sees an append';

$b.unshift(0);
is $b.AT-POS(0), 0, 'unshift puts the value at the front';
is $view.raku, '(0, 1, 2, 3, 4)', 'and the earlier List sees it';

$b.prepend(ib(-2, -1));
is $b.List.raku, '(-2, -1, 0, 1, 2, 3, 4)', 'prepend puts the elements at the front';

$b.BIND-POS(9, 'x');
is $b.elems, 10, 'BIND-POS past the end grows the buffer';
is $b.AT-POS(9), 'x', 'and binds the element';
ok $b.AT-POS(8) =:= Mu, 'a grown slot reads back as Mu';
ok $b.AT-POS(42) =:= Mu, 'AT-POS past the end is Mu';

$b.append($b);
is $b.elems, 20, 'appending the buffer to itself doubles it';

$b.clear;
is $b.elems, 0, 'clear empties the buffer';
is $view.elems, 0, 'and the earlier List sees it';

# Many appends stay linear: one buffer gathering batches.
my $acc := IterationBuffer.new;
my $ten := ib(|^10);
$acc.append($ten) for ^2000;
is $acc.elems, 20000, 'repeated appends gather every batch';
is $acc.AT-POS(19999), 9, 'the last batch is at the end';
