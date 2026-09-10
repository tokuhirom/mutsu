use Test;

plan 6;

# Binding to a zen slice (`@a[]` / `%a{}`) is illegal: X::Bind::ZenSlice.
throws-like 'my @a; @a[] := <foo bar baz>', X::Bind::ZenSlice, type => Array;
throws-like 'my %a; %a{} := foo=>1, bar=>2, baz=>3', X::Bind::ZenSlice, type => Hash;
throws-like 'my @a; @a[] := 1', X::Bind::ZenSlice;
throws-like 'my %h; %h{} := 1', X::Bind::ZenSlice;

# Ordinary container binding must still work (not a zen slice).
{
    my @a = 1, 2, 3;
    my @b := @a;
    is @b.elems, 3, 'plain array binding still works';
}
{
    my %h = a => 1;
    my %g := %h;
    is %g<a>, 1, 'plain hash binding still works';
}
