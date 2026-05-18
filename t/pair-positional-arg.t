use Test;
plan 6;

# Pair from variable should be a positional argument, not named
sub f1($a, $b) { $a.WHAT.gist ~ ':' ~ $b }
sub f2($a, Str:D $b) { $a.WHAT.gist ~ ':' ~ $b }
sub f3($a, Str $b) { $a.WHAT.gist ~ ':' ~ $b }

my $p = name => 'Alice';

is f1($p, 'test'), '(Pair):test', 'Pair from variable as positional (untyped)';
is f2($p, 'test'), '(Pair):test', 'Pair from variable as positional (Str:D)';
is f3($p, 'test'), '(Pair):test', 'Pair from variable as positional (Str)';

# Pair in map context
sub visit($context, Str:D $field) {
    $context ~~ Associative ?? ($context{$field} // '') !! '';
}
my @context = (name => 'Alice', age => 30);
my @results = @context.map({visit($^ctx, 'name')});
is @results.elems, 2, 'map with Pair args works';
is @results[0], 'Alice', 'Pair subscript access in visit';

# Named parameter should still work
sub f4(:$name) { $name }
is f4(name => 'test'), 'test', 'literal pair as named arg still works';
