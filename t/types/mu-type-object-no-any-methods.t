use Test;

# `Mu` is not an `Any`: its type object has none of Any's list methods.

my @names = <List list Array first elems sum join unique minmax tail head pick roll flat>;
plan @names.elems + 2;

for @names -> $m {
    throws-like { Mu."$m"() }, X::Method::NotFound, "Mu.$m is X::Method::NotFound";
}

is Any.elems, 1, 'Any.elems still answers';
is Mu.defined, False, 'Mu keeps its own methods';
