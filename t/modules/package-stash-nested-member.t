use Test;

# A package stash lists a nested package's term as that sub-package, never
# under the sub-package's name with the term's value (#17475; CompUnit::Util's
# `set-in-WHO` walks a stash this way).

plan 6;

my package tmp {};
sub walk($WHO is copy) {
    $WHO.package_at_key('Foo');
    $WHO = $WHO<Foo>.WHO;
    $WHO.package_at_key('Bar');
    $WHO = $WHO<Bar>.WHO;
    $WHO<$Baz> := 'bar';
}
BEGIN walk(tmp.WHO);

is tmp::Foo::Bar::<$Baz>, 'bar', 'the variable is reachable by its qualified stash';
is tmp.WHO<Foo>.^name, 'tmp::Foo', 'the outer stash entry is still the outer package';
is tmp.WHO<Foo>.WHO<Bar>.^name, 'tmp::Foo::Bar', 'the inner entry is the inner package';
is tmp.WHO.keys.sort.join(','), 'Foo', 'the outer stash lists only the sub-package';
is tmp::<Foo>.^name, 'tmp::Foo', 'a keyed read of the outer stash gives the outer package';
is tmp.WHO<Foo>.WHO<Bar>.WHO<$Baz>, 'bar', 'walking the stashes reaches the variable';
