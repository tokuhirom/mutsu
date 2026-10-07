use Test;

plan 2;

# #12282: `my $a` declared as a method-call argument, captured by a closure,
# must not alias a later same-named declaration in another loop.
class H { method add($m) { $m } }
my $h = H.new;
sub keep($x) { $x }
my @r;
for <x y> -> $n {
    $h.add: my $a = $n;
    @r.push: -> { $a };
}
is-deeply @r.map({ .() }).List, ("x", "y"), 'closure over method-arg decl keeps its own value';
for <x y> -> $n {
    keep my $a = $n;
}
is-deeply @r.map({ .() }).List, ("x", "y"), 'later same-named call-arg decl does not disturb it';
