use Test;

plan 10;

# `:delete` on a *nested* subscript (`%h<a><x>:delete`) must delete from the
# inner container and write it back — the generic path deleted from a copy and
# lost it, so the key survived even though the deleted value was returned.

{
    my %h = a => {x => 1, y => 2};
    %h<a><x>:delete;
    is-deeply %h, {a => {y => 2}}, 'nested <><>:delete removes the inner key';
}
{
    my %h = a => {x => 1, y => 2};
    %h<a>{"x"}:delete;
    is-deeply %h, {a => {y => 2}}, 'nested <>{}:delete removes the inner key';
}
{
    my %h = a => {b => {c => 1, d => 2}};
    %h<a><b><c>:delete;
    is-deeply %h, {a => {b => {d => 2}}}, 'three-level :delete removes the deepest key';
}
{
    my %h = a => {x => 1, y => 2};
    my $got = %h<a><x>:delete;
    is $got, 1, 'nested :delete returns the deleted value';
    is-deeply %h<a>, {y => 2}, '...and mutates the inner hash';
}
{
    my @a = [1, 2, 3], [4, 5, 6];
    @a[0][1]:delete;
    is-deeply @a, [[1, Any, 3], [4, 5, 6]], 'nested array :delete leaves an Any hole';
}
{
    my %h = a => {only => 42};
    my $got = %h<a><only>:delete;
    is $got, 42, 'deleting the last inner key returns it';
    is-deeply %h, {a => {}}, '...leaving an empty inner hash';
}
# single-level and slice forms are unaffected
{
    my %h = x => 1, y => 2;
    %h<x>:delete;
    is-deeply %h, {y => 2}, 'single-level :delete still works';
}
{
    my %h = a => 1, b => 2, c => 3;
    %h<a b>:delete;
    is-deeply %h, {c => 3}, 'slice :delete still works';
}
