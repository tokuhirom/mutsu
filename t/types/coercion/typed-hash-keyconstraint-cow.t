use Test;

# Regression: a typed (object) hash `my %h{Int}` must keep its key constraint
# (and therefore .WHICH-keyed object-hash semantics) across copy-on-write and
# across earlier exited blocks that declared a same-named hash with a different
# constraint. The constraint lives in Arc-pointer-keyed metadata that COW and
# Arc-pointer reuse could intermittently drop, degrading `%h{$int}` reads to
# string-keyed lookups that returned Nil (roast S02-types/hash.t,
# S09-typed-arrays/hashes.t, previously mislabeled "CI-load timeouts").

plan 8;

# An earlier exited block declares `%h` with a *different* constraint, so its
# stale metadata is in play when the object hash below is declared.
{
    my Int %h;
    %h = (a => 3, b => 7);
    %h<foo> = 8;
}

{
    my %h{Int};
    is %h.keyof.^name, 'Int', 'keyof is Int after a prior differently-typed %h block';

    # Failing string-key assignments (these COW the hash) must not drop the
    # key constraint for the subsequent valid Int-key assignments.
    dies-ok { %h<a> = 1 }, 'string key rejected';
    dies-ok { %h<a b c> = (1, 2, 3) }, 'string key slice rejected';

    %h{1} = "a";
    is %h{1}, 'a', 'single Int-key assignment persists after COW';

    %h{(2, 3, 4)} = <b c d>;
    is %h{2}, 'b', 'Int-key slice assignment persists (key 2)';
    is %h{4}, 'd', 'Int-key slice assignment persists (key 4)';

    ok %h{2}:exists, ':exists works on the object hash';
    is %h.keyof.^name, 'Int', 'keyof still Int after assignments';
}
