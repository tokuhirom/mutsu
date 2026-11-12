use Test;

# A routine whose only inner code is closures is called through the
# positional-light path, in a frame of its own (#17497). What a closure created
# there sees and keeps must not change: Monad's type helpers are this shape
# (`absent(hit) ?? publish-type(key, { ... }) !! hit`).

plan 12;

sub with-block(&b) { b() }

sub pick(Mu \M, Mu \T) { M ?? M !! with-block({ T }) }
is pick(5, Int), 5, 'the plain branch';
is pick(0, Str).^name, 'Str', 'the closure reads a sigilless parameter';

sub make-adder($n) { return -> $x { $x + $n } }
my &add2 = make-adder(2);
my &add5 = make-adder(5);
is add2(1), 3, 'an escaping closure keeps its own parameter';
is add5(1), 6, 'a second call made a second frame';

sub count-up($start) { my $c = $start; with-block({ $c++ }); with-block({ $c++ }); $c }
is count-up(3), 5, 'a closure writes a local of the routine';

my $outer = 10;
sub add-outer($y) { with-block({ $outer + $y }) }
is add-outer(5), 15, 'a closure reads an outer lexical and a parameter';

sub bump-outer($y) { with-block({ $outer += $y }) }
bump-outer(2);
is $outer, 12, 'a closure writes an outer lexical';

sub first-even(@xs) { @xs.map({ return $_ if $_ %% 2 }); 'none' }
is first-even([1, 3, 4, 5]), 4, 'return from a block returns from the routine';
is first-even([1, 3]), 'none', '...and falls through when it does not';

sub fact($n) { $n <= 1 ?? 1 !! with-block({ $n * fact($n - 1) }) }
is fact(6), 720, 'recursion keeps each frame its own';

my $x = 'caller';
sub shadow($x) { with-block({ $x }) }
is shadow('param'), 'param', 'the closure sees the parameter, not the caller';
is $x, 'caller', "the caller's same-named variable is untouched";
