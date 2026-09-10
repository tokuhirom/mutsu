use Test;

# §1.5 slice S6: `%h<k>:delete` / `@a[i]:delete` write the mutated container back
# through the compile-time-baked local slot (DeleteIndexNamed), not a by-name
# `code.locals` search.

plan 9;

# basic hash / array element delete on a local
{
    my %h = a => 1, b => 2, c => 3;
    is (%h<b>:delete), 2, 'hash delete returns the removed value';
    is-deeply %h.sort.list, (:a(1), :c(3)), 'the local hash reflects the delete';
}
{
    my @a = 10, 20, 30, 40;
    @a[1]:delete;
    is-deeply @a[0], 10, 'array delete keeps element 0';
    ok !@a[1].defined, 'array delete clears element 1';
    is-deeply @a[2], 30, 'array delete keeps element 2';
}

# a shadowing inner-block delete mutates the inner hash; the outer is intact
{
    my %h = a => 1;
    {
        my %h = x => 1, y => 2;
        %h<x>:delete;
        is-deeply %h.sort.list, (:y(2),), 'delete of an inner shadow writes the inner slot';
    }
    is-deeply %h.sort.list, (:a(1),), 'the outer hash is untouched by the shadow delete';
}

# a `:=` alias observes the delete through the shared binding
{
    my %h = a => 1, b => 2;
    my %g := %h;
    %g<a>:delete;
    is-deeply %h.sort.list, (:b(2),), ':= source hash after delete';
    is-deeply %g.sort.list, (:b(2),), ':= alias reflects the delete';
}
