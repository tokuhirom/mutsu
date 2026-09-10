use Test;

plan 11;

# A `for` block owns its topic: raku binds `$_` as the block's own implicit
# parameter, so the enclosing `$_` is untouched once the loop ends. mutsu used
# to leak the loop topic for every shape except a topic-taking loop with a
# single-statement body — most visibly, a nested `for LIST -> $a, $b { }` left
# the outer `$_` bound to the last *chunk* (a List), so a following `$_<key>`
# in the outer body died with "Type Array does not support associative
# indexing".

for ({a => 1},) {
    for 1, 2 { }
    is $_.^name, 'Hash', 'a topic-taking inner for restores the outer topic';
    is $_<a>, 1, 'the outer topic is still subscriptable afterwards';
}

for ({a => 1},) {
    for 1, 2 -> $x { }
    is $_.^name, 'Hash', 'a one-param inner for leaves the outer topic alone';
}

for ({a => 1},) {
    for 1, 2, 3, 4 -> $x, $y { }
    is $_.^name, 'Hash', 'a two-param inner for restores the outer topic';
    is $_<a>, 1, 'the outer topic is not the last iteration chunk';
}

for ({a => 1},) {
    my @l = 1, 2, 3, 4;
    for @l -> $x, $y { }
    is $_.^name, 'Hash', 'an array-source two-param for restores the outer topic';
}

# A multi-statement body is restored too (the old heuristic only restored a
# single-statement body).
for ({a => 1},) {
    for 1, 2 { my $t = $_; my $u = $t + 1 }
    is $_.^name, 'Hash', 'a multi-statement inner for restores the outer topic';
}

# Nesting more than one level deep unwinds in order.
for ({a => 1},) {
    for 'x', 'y' {
        for 1, 2, 3, 4 -> $p, $q { }
        is $_.^name, 'Str', 'the middle loop keeps its own topic';
    }
    is $_.^name, 'Hash', 'the outermost topic survives two levels of nesting';
}

# At file scope `$_` starts undefined, and a loop must not leave it set.
{
    my $before = $_;
    for 1, 2, 3 { }
    is $_.WHICH, $before.WHICH, 'a mainline for does not leave $_ set';
}
