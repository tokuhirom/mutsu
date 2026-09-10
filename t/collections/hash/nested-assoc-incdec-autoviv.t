use Test;

plan 12;

# `++`/`--`/`OP=` on a nested *associative* subscript must autovivify the
# intermediate container as a Hash (matching the `= value` path), not an Array.
# The classic nested-counter idiom `%h<a>{$k}++` relied on this.

{
    my %h;
    %h<a><b>++;
    is-deeply %h, {a => {b => 1}}, 'nested <><>++ autovivifies a Hash';
}
{
    my %h;
    %h<a>{"b"}++;
    is-deeply %h, {a => {b => 1}}, 'nested <>{}++ autovivifies a Hash';
}
{
    my %h;
    %h{"x"}{"y"}++;
    is-deeply %h, {x => {y => 1}}, 'nested {}{}++ autovivifies a Hash';
}
{
    my %h;
    %h<a><b><c>++;
    is-deeply %h, {a => {b => {c => 1}}}, 'three-level ++ autovivifies Hashes';
}
{
    my %h;
    ++%h<a><b>;
    is-deeply %h, {a => {b => 1}}, 'prefix ++ on a nested associative subscript';
}
{
    my %h;
    %h<a><b>--;
    is-deeply %h, {a => {b => -1}}, 'nested -- autovivifies a Hash';
}
{
    my %h;
    %h<a><b> += 5;
    is-deeply %h, {a => {b => 5}}, 'nested += autovivifies a Hash';
}
{
    my %h;
    %h<a><b> ~= "x";
    is-deeply %h, {a => {b => "x"}}, 'nested ~= autovivifies a Hash';
}
{
    # the classic nested-counter idiom
    my %h;
    %h<counts>{$_}++ for <a b a c a>;
    is-deeply %h<counts>.sort.list, (a => 3, b => 1, c => 1), 'nested-counter %h<c>{$k}++ works';
}
{
    my %h;
    %h<x>{$_} += 1 for <a b a>;
    is-deeply %h<x>.sort.list, (a => 2, b => 1), 'nested-counter with += works';
}
# an existing nested value is updated, not replaced
{
    my %h = a => {b => 1};
    %h<a><b> += 10;
    is-deeply %h, {a => {b => 11}}, 'nested += updates an existing value';
}
# positional nested compound-assign is unaffected
{
    my @a;
    @a[0][1] += 3;
    is-deeply @a, [[Any, 3],], 'positional nested += autovivifies an Array';
}
