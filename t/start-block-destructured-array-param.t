use Test;

# An `@`/`%` parameter bound by a destructuring sub-signature is a fresh
# per-invocation binding, but it was left on the name-keyed `shared_vars` lane
# that `start` blocks read from. That lane is seeded once per name, so every
# worker spawned by a later iteration read the FIRST iteration's value:
#
#   await map -> [$a, @K] { start { "$a:{@K[0]}" } }, (1, (100,101)), (2, (200,201))
#   # was 1:100,2:100
#
# `$`-sigil sub-parameters were already correct (the closure machinery owns them
# per binding), and so were plain `-> @K { ... }` parameters.

plan 8;

{
    my @got = await map -> [$a, @K] { start { "$a:{@K[0]}" } },
        (1, (100, 101)), (2, (200, 201));
    is @got.join('|'), '1:100|2:200',
        'each worker sees its own destructured @ parameter';
}

{
    my @got = await map -> [@K] { start { @K.join(',') } },
        ((10, 11),), ((20, 21),), ((30, 31),);
    is @got.join('|'), '10,11|20,21|30,31', 'three workers, three bindings';
}

{
    my @got = await map -> [%h] { start { %h<k> } },
        (%(k => 'a'),), (%(k => 'b'),);
    is @got.join('|'), 'a|b', 'a destructured % parameter too';
}

# A slow body makes the workers genuinely overlap.
{
    sub slow { my $s = 0; $s += $_ for ^20000; $s }
    my @got = await map -> [$a, @K] { start { slow(); "$a:{@K[0]}" } },
        (1, (100, 101)), (2, (200, 201));
    is @got.join('|'), '1:100|2:200', 'still correct when the workers overlap';
}

# The shapes that already worked must keep working.
{
    # A distinct name: the `@`-keyed shared-var lane cannot hold two live
    # bindings of ONE name, so reusing `@K` here would hit a separate,
    # pre-existing bug (recorded in todo/tickets/).
    my @got = await map -> @P { start { @P[0] } }, (100, 101), (200, 201);
    is @got.join('|'), '100|200', 'a plain @ parameter is unaffected';
}

{
    my @got = map -> [$a, @K] { "$a:{@K[0]}" }, (1, (100, 101)), (2, (200, 201));
    is @got.join('|'), '1:100|2:200', 'the synchronous form is unaffected';
}

{
    my @got = (map -> [$a, @K] { -> { "$a:{@K[0]}" } }, (1, (100, 101)), (2, (200, 201)))
        .map({ .() });
    is @got.join('|'), '1:100|2:200', 'a plain closure is unaffected';
}

# An ordinary outer array still reaches a worker as ONE shared object.
{
    my @shared = 1, 2, 3;
    await start { @shared.push(4) };
    is @shared.join(','), '1,2,3,4', 'an outer @ lexical is still shared with a worker';
}
