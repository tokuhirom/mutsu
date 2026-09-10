use v6;
use Test;

# Passing a container to a Raku-level routine boxes its slot into a shared
# container cell. Every later mutation of that variable must still be visible
# through the variable itself AND through any alias of it. These all silently
# no-op'd (or detached the alias) before the `with_deref`-blind read/write sites
# behind `Hash.push`/`Hash.append` and the `:delete`-with-adverb companion were
# routed through the cell-descending chokepoint.

plan 33;

sub peek(Mu $got) { }

# --- Hash.push / Hash.append after the hash has been passed as an argument ---

{
    my %h;
    %h.push: 'b', 2, 'a', 1, 'c', 3;
    is-deeply %h, {a => 1, b => 2, c => 3}, 'push before any argument binding';
    peek(%h);
    %h.push: (:a(4), :a(5));
    is-deeply %h, {a => [1, 4, 5], b => 2, c => 3},
        'stacking push of a pair list survives argument binding';
}

{
    my %h;
    %h.push: 'b', 2;
    peek(%h);
    %h.push: 'c', 3;
    is-deeply %h, {b => 2, c => 3}, 'alternating key/value push survives argument binding';
}

{
    my %h = x => 1;
    peek(%h);
    %h.push: (:y(2), :z(3));
    is-deeply %h, {x => 1, y => 2, z => 3}, 'pair-list push survives argument binding';
}

{
    my %h = x => 1;
    peek(%h);
    %h.push: {y => 2};
    is-deeply %h, {x => 1, y => 2}, 'hash-argument push survives argument binding';
}

{
    my %h = k => [1, 2];
    peek(%h);
    %h.append: 'k', [3, 4];
    is-deeply %h, {k => [1, 2, 3, 4]}, 'append flattens after argument binding';
}

{
    my %h = k => 1;
    peek(%h);
    %h.push: (k => 2);
    is-deeply %h, {k => [1, 2]}, 'duplicate-key push stacks after argument binding';
}

{
    my %h = x => 1;
    peek(%h);
    %h.push('y', 2);
    is-deeply %h, {x => 1, y => 2}, 'parenthesised push call survives argument binding';
}

{
    my %h = x => 1;
    peek(%h);
    %h.append('y', 2);
    is-deeply %h, {x => 1, y => 2}, 'parenthesised append call survives argument binding';
}

{
    my %h = x => 1;
    peek(%h);
    push %h, 'y', 2;
    is-deeply %h, {x => 1, y => 2}, 'push listop survives argument binding';
}

{
    my %h = x => 1;
    peek(%h);
    my $name = 'push';
    %h."$name"('y', 2);
    is-deeply %h, {x => 1, y => 2}, 'indirect push call survives argument binding';
}

# --- the same, observed through an alias ---

{
    my %h = x => 1;
    my %alias := %h;
    peek(%h);
    %h.push('y', 2);
    is-deeply %alias, {x => 1, y => 2}, 'alias sees a push made after argument binding';
    is-deeply %h, %alias, 'the hash and its alias stay the same value';
}

{
    my %h = x => 1;
    my %alias := %h;
    peek(%h);
    push %h, 'y', 2;
    is-deeply %alias, {x => 1, y => 2}, 'alias sees a listop push made after argument binding';
}

{
    my %h = a => 1;
    my $bound := %h;
    peek($bound);
    $bound.push('b', 2);
    is-deeply %h, {a => 1, b => 2}, 'scalar-bound hash push reaches the bind source';
}

# --- `:delete` combined with a `:k`/`:v`/`:p`/`:kv` adverb ---

{
    my %h = a => 1, b => 2;
    peek(%h);
    my $got = %h<a>:delete;
    is-deeply $got, 1, 'plain :delete still answers the removed value';
    is-deeply %h, {b => 2}, 'plain :delete removes after argument binding';
}

{
    my %h = a => 1, b => 2;
    peek(%h);
    my $got = %h<a>:delete:p;
    is-deeply $got, (a => 1), ':delete:p answers the removed pair';
    is-deeply %h, {b => 2}, ':delete:p actually removes after argument binding';
}

{
    my %h = a => 1, b => 2;
    peek(%h);
    my @got = (%h<a c>:delete:p).list;
    is-deeply @got, [a => 1], ':delete:p slice answers the present pairs';
    is-deeply %h, {b => 2}, ':delete:p slice removes after argument binding';
}

{
    my %h = a => 1, b => 2;
    peek(%h);
    %h<a b>:delete:k;
    is-deeply %h, {}, ':delete:k slice removes after argument binding';
}

{
    my %h = a => 1, b => 2;
    my %alias := %h;
    peek(%h);
    %h<a>:delete:p;
    is-deeply %alias, {b => 2}, 'alias sees a :delete:p made after argument binding';
}

# --- the positional twin ---

{
    my @a = 1, 2, 3;
    peek(@a);
    @a[0, 1]:delete:p;
    is-deeply @a, [Any, Any, 3], ':delete:p on a positional slice removes after argument binding';
}

{
    my @a = 1, 2, 3;
    my @alias := @a;
    peek(@a);
    @a[0]:delete;
    is-deeply @alias, [Any, 2, 3], 'alias sees an array :delete made after argument binding';
}

# --- the by-value hash push implementation agrees with the lvalue one ---

{
    my %h = a => 1;
    my $copy = %h.push('b', 2);
    is-deeply $copy, {a => 1, b => 2}, 'Hash.push returns the merged hash';
}

{
    my %h = k => [1, 2];
    my $copy = %h.append('k', [3, 4]);
    is-deeply $copy, {k => [1, 2, 3, 4]}, 'Hash.append flattens in the returned hash';
}

{
    my %h;
    peek(%h);
    %h.push: 'a', 1, 'a', 2;
    is-deeply %h, {a => [1, 2]}, 'repeated key in one push stacks after argument binding';
}

# --- the richer typed/object-hash semantics survive argument binding too ---

{
    my %h{Int};
    peek(%h);
    %h.push(1, 'x');
    is-deeply %h.keys.list, (1,), 'object-hash push keeps the typed key after argument binding';
    is-deeply %h{1}, 'x', 'object-hash push stores under the typed key';
}

{
    my Int %h = a => 1;
    peek(%h);
    dies-ok { %h.push('b', 'not-an-int') },
        'typed-hash push still type-checks after argument binding';
}

{
    my %h is default(42) = a => 1;
    peek(%h);
    %h.push('b', 2);
    is-deeply %h, {a => 1, b => 2}, 'is default(...) hash pushes after argument binding';
    is-deeply %h<zz>, 42, 'is default(...) survives the push';
}
