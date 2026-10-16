use Test;

# A built-in iterator's protocol methods are rows of the `Iterator` and
# `PredictiveIterator` roles (ADR-11276 §9.126): one implementation, whatever
# the receiver is (a variable, an array element, a temporary), and `.can`
# answers what the iterator's kind does.

plan 26;

# An array-backed iterator in a variable.
{
    my $it = (1, 2, 3, 4, 5).iterator;
    is $it.pull-one, 1, 'pull-one';
    is $it.skip-one, 1, 'skip-one skips';
    is $it.count-only, 3, 'count-only counts what is left';
    my @buf;
    is $it.push-exactly(@buf, 2), 2, 'push-exactly answers how many it pushed';
    is-deeply @buf, [3, 4], 'push-exactly pushed them';
    is $it.skip-at-least-pull-one(0), 5, 'skip-at-least-pull-one(0) pulls the next';
    ok $it.pull-one =:= IterationEnd, 'exhausted';
    nok $it.bool-only, 'bool-only of an exhausted iterator';
}

# The same iterator held in an array element advances in place.
{
    my @its = (1, 2, 3).iterator, (7, 8).iterator;
    is @its[0].pull-one, 1, 'element: first pull';
    is @its[0].pull-one, 2, 'element: second pull sees the first';
    is @its[1].skip-at-least(1), 1, 'element: skip-at-least';
    is @its[1].pull-one, 8, 'element: the skip advanced the iterator';
}

# A temporary receiver.
is (10, 20).iterator.pull-one, 10, 'a temporary iterator pulls';

# A squish scan is an Iterator but not a PredictiveIterator, as in Rakudo.
{
    my $it = (1, 1, 2, 2, 3).squish(:as(* + 0)).iterator;
    nok $it.can('count-only'), 'a squish iterator cannot count-only';
    ok $it.can('pull-one'), 'a squish iterator can pull-one';
    is $it.pull-one, 1, 'squish pull-one';
    my @rest;
    $it.push-all(@rest);
    is-deeply @rest, [2, 3], 'squish push-all';
}
{
    my @its = (1, 1, 2).squish(:as(* % 2)).iterator,;
    is @its[0].pull-one, 1, 'squish in an array element: first';
    is @its[0].pull-one, 2, 'squish in an array element: second';
}

# A .map stream and a gather are not predictive either.
nok (1, 2).map(* + 1).iterator.can('count-only'), 'a map iterator cannot count-only';
nok (gather { take 1 }).iterator.can('bool-only'), 'a gather iterator cannot bool-only';
ok (1, 2).iterator.can('count-only'), 'a list iterator can count-only';
throws-like { (1, 1).squish(:as(&abs)).iterator.count-only }, X::Method::NotFound,
    'count-only of a squish iterator is not found';

# A lazy source is topped up as far as the call needs.
{
    my $it = (1 .. *).iterator;
    is $it.pull-one, 1, 'unending range: pull-one';
    is $it.skip-at-least-pull-one(2), 4, 'unending range: skip-at-least-pull-one';
    nok $it.can('count-only'), 'an unending range iterator cannot count-only';
}
