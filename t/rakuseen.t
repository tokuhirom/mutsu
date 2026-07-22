use Test;

# `Mu.rakuseen($id, &code)` is the cyclic-structure guard used by `.raku`/`.gist`.
# First sight of an id runs `&code` and returns its rendering; a repeat of the
# same id (a reference cycle) returns a backreference name instead of recursing
# forever, and the outer occurrence binds it as `(my \NAME = ...)`.

plan 6;

# Non-cyclic: just runs the block and returns its value.
is 42.rakuseen("id1", { "BODY" }), 'BODY', 'rakuseen runs &code and returns it';
is "x".rakuseen("id2", { 1 + 2 }), 3, 'rakuseen returns the block value (non-Str)';

# Distinct ids do not interfere.
is-deeply
    (7.rakuseen("a", { "A" }), 7.rakuseen("b", { "B" })),
    ("A", "B"),
    'distinct ids each run their block';

# The same id used sequentially (not nested) is not a cycle.
is 1.rakuseen("seq", { "one" }) ~ 1.rakuseen("seq", { "two" }), 'onetwo',
    'the same id used sequentially is not treated as a cycle';

# A self-referential structure terminates (no infinite recursion) and produces a
# `(my \NAME = ...)` backreference binding rather than hanging.
class Node {
    has $.next is rw;
    method raku {
        self.rakuseen("Node" ~ self.WHICH, {
            "Node(" ~ ($.next ?? $.next.raku !! "Nil") ~ ")"
        })
    }
}
my $n = Node.new;
$n.next = $n;                          # cycle
my $r = $n.raku;
ok $r.contains('my \\'), 'a cyclic .raku emits a (my \NAME = ...) backreference';
ok $r.contains('Node('), 'the cyclic .raku still renders the outer node body';
