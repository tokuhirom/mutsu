use Test;
plan 15;

# `class ... is Hash` subclass with no user-defined `new`. The instance must
# behave as an Associative: subscripting, list context, gist, and coercion
# methods all delegate to the backing storage. Mirrors
# `t/array-subclass-vector.t` for the Associative twin of the delegation
# subsystem (see todo/deep/hash-subclass-instance-has-no-method-delegation.md
# / news/2026-08/hash-subclass-instance-delegation.md).
class Registry is Hash {
    method count () { self.elems }
}

my $r = Registry.new(a => 1, b => 2, c => 3);
is $r.WHAT.gist, '(Registry)', 'scalar bind keeps Registry type';
is $r<a>, 1, 'AT-KEY reads element a';
is $r<c>, 3, 'AT-KEY reads element c';
is $r.elems, 3, 'elems delegates to storage';
is $r.count, 3, 'a user method calling self.elems reaches storage';

my %r-wrong = $r;
is %r-wrong.^name, 'Hash', 'flattening assignment to a %-sigil variable loses the subclass (mirrors Array)';

is $r.gist, '{a => 1, b => 2, c => 3}', 'gist shows the elements';
is $r.keys.sort.List, ('a', 'b', 'c').List, 'keys delegates to storage';

# ASSIGN-KEY via direct method call.
{
    my $reg = Registry.new(x => 1);
    $reg.ASSIGN-KEY('y', 2);
    is $reg<y>, 2, 'ASSIGN-KEY via method call';
}

# direct subscript assignment and post-inc/dec on a scalar-held instance.
{
    my $reg = Registry.new(a => 1);
    $reg<a> = 99;
    is $reg<a>, 99, 'direct subscript assign writes storage';
    $reg<a>--;
    is $reg<a>, 98, 'subscript post-decrement writes storage';
    $reg<a>++;
    is $reg<a>, 99, 'subscript post-increment writes storage';
}

# list context iterates the pairs, not the instance as one item.
{
    my $reg = Registry.new(a => 1, b => 2);
    my @seen;
    @seen.push($_.key) for $reg.sort;
    is-deeply @seen, ['a', 'b'], 'for $reg.sort iterates storage pairs';
}

# push still works and grows storage, returning the invocant.
{
    my $reg = Registry.new(a => 1);
    my $out = $reg.push: (b => 2);
    is $reg.elems, 2, 'push grows storage';
    ok $out === $reg, 'push returns the invocant';
}
