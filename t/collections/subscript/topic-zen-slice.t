use v6;
use Test;

# `.<>` is the zen slice on the topic — `$_<>`, the whole container. The topic
# parser has its own copy of each subscript (`.<key>`, `.[i]`, `.{k}`), and the
# angle one required at least one key, so the zero-width spelling reached no
# branch and the term failed to parse:
#
#     self!run-test: |.<> for $test.subs      # Test::Describe::Expect
#     prepare-param($_, %pars).() for .<>     # Test::Describe::It
#
# It now yields the bare topic with the subscript unconsumed, so the undotted
# postfix branches parse it — the same rewind that gave `$x.[0; 1]` and `$x.<>`
# their full set of spellings — which also brings the `:k`/`:v`/`:kv`/`:p`
# adverbs along for free.

plan 9;

{
    my @a = 1, 2, 3;
    with @a { is .<>.elems, 3, '.<> on an array topic is the whole array' }
    given @a { is .<>.join(','), '1,2,3', 'and it keeps the elements' }
}
{
    my %h = a => 1, b => 2;
    with %h { is .<>.elems, 2, '.<> on a hash topic is the whole hash' }
    with %h { is-deeply .<>:k.sort.list, ('a', 'b'), 'a `:k` adverb on the topic zen slice' }
    with %h { is-deeply .<>:v.sort.list, (1, 2), 'and `:v`' }
}
{
    my $x = (1, 2);
    given $x { is-deeply .<>, (1, 2), '.<> on a scalar topic decontainerizes it' }
}

# In the shape the distributions use: as a `for` list, and flattened into a call.
{
    my @seen;
    my @items = 'a', 'b';
    with @items {
        @seen.push: $_ for .<>;
    }
    is @seen.join(','), 'a,b', '.<> as a `for` statement-modifier list';
}
{
    sub take-two($x, $y) { "$x/$y" }
    my @pair = 'l', 'r';
    with @pair { is take-two(|.<>), 'l/r', '|.<> flattens the topic into a call' }
}

# The keyed spellings are unchanged.
{
    my %h = k => 'v';
    with %h { is .<k>, 'v', '.<key> still looks up a key' }
}
