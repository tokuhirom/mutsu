use v6;
use Test;

# Once a `push` routes a plain lexical array into the cross-thread
# `__mutsu_atomic_arr::` store (any mutation on a worker thread does this),
# every other mutating array op must go through the same store: reads prefer
# the atomic entry, so a mutation applied to the stale base copy is silently
# lost. The zef `populate-distributions` bug: on a hyper worker,
# `push @idx, $name; append @idx, @provides` lost every appended element,
# so ~80% of `%!short-name-lookup` keys vanished.

plan 12;

my @one = (1,);

sub on-hyper-worker(&body) {
    @one.hyper(:batch(1)).map(-> $x { body() }).List[0];
}

is on-hyper-worker({ my @a; push @a, "n"; append @a, ("p1", "p2"); @a.join(",") }),
    "n,p1,p2", "append (listop) after push lands on a hyper worker";

is on-hyper-worker({ my @a; push @a, "n"; @a.append: ("p1", "p2"); @a.join(",") }),
    "n,p1,p2", "append (method) after push lands on a hyper worker";

is on-hyper-worker({ my @a; push @a, "n"; prepend @a, ("p1", "p2"); @a.join(",") }),
    "p1,p2,n", "prepend after push lands on a hyper worker";

is on-hyper-worker({ my @a; push @a, "n"; unshift @a, "p0"; @a.join(",") }),
    "p0,n", "unshift after push lands on a hyper worker";

is on-hyper-worker({ my @a; push @a, "n", "m"; my $p = @a.pop; "{@a.join(',')}|$p" }),
    "n|m", "pop after push removes the element on a hyper worker";

is on-hyper-worker({ my @a; push @a, "n", "m"; my $s = @a.shift; "{@a.join(',')}|$s" }),
    "m|n", "shift after push removes the element on a hyper worker";

is on-hyper-worker({ my @a; push @a, "n"; @a.splice(0, 0, "p1"); @a.join(",") }),
    "p1,n", "splice insertion after push lands on a hyper worker";

is on-hyper-worker({ my @a; push @a, "n", "m"; my @r = @a.splice(0, 1); "{@a.join(',')}|{@r.join(',')}" }),
    "m|n", "splice removal after push lands on a hyper worker";

is on-hyper-worker({ my @a; append @a, ("p1", "p2"); push @a, "n"; @a.join(",") }),
    "p1,p2,n", "append before push also lands on a hyper worker";

is on-hyper-worker({
    my @a;
    push @a, "n";
    @a.pop;
    my $r = @a.pop;
    $r.defined ?? "defined" !! "undefined"
}), "undefined", "pop on emptied shared array still returns a Failure";

# The original shape: statement-modifier for over a Seq mixing pushed and
# appended keys, accumulated into a hash (the zef short-name index).
is on-hyper-worker({
    my %lookup;
    for <A B> -> $name {
        my @idx;
        push @idx, $name;
        append @idx, ("{$name}::P1", "{$name}::P2");
        push %lookup{$_}, $name for @idx.grep(*.so).unique;
    }
    %lookup.keys.elems
}), 6, "hash keyed by pushed+appended names sees every key (zef populate shape)";

# start-block workers exercise the same store.
is (await start { my @a; push @a, "n"; append @a, ("p1", "p2"); @a.join(",") }),
    "n,p1,p2", "append after push lands inside start block";

done-testing;
