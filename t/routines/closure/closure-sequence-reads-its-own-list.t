use Test;

# A closure sequence runs its generator on demand, so the generator can read
# the array the sequence is being assigned to (Math::Sequences'
# Hofstadter G: `our @A005206 = 0, {++$ - @A005206[@A005206[$++]]} … ∞`),
# (single-letter names: a longer lowercase name reads a snapshot, #15387),
# and a lazy array's clone shares the generator with the original.

plan 8;

{
    our @G = 0, {++$ - @G[@G[$++]]} … ∞;
    is-deeply @G[^12].List, (0, 1, 1, 2, 3, 3, 4, 4, 5, 6, 6, 7), 'Hofstadter G reads its own array (our)';
}

{
    our @H = 0, {++$ - @H[@H[$++]]} … *;
    is-deeply @H[^12].List, (0, 1, 1, 2, 3, 3, 4, 4, 5, 6, 6, 7), 'and with a star endpoint';
}

{
    our @K = 0, {@K[0] + 1} … *;
    is-deeply @K[^4].List, (0, 1, 1, 1), 'a generator reading its own array does not deadlock';
}

{
    our @L = 0, {@L[$++] + 1} … *;
    is-deeply @L[^4].List, (0, 1, 2, 3), 'elements generated in one pull are visible to the next step';
}

{
    my $n = 0;
    my @c = 1, { $n++; $_ + 1 } … *;
    is $n, 0, 'the generator does not run ahead of demand';
    is-deeply @c[^3].List, (1, 2, 3), 'and produces the elements on demand';
    is $n, 2, 'one call per generated element';
}

{
    my @a = 1, {rand} … *;
    my @b = @a.clone;
    is-deeply @a[^20].List, @b[^20].List, 'clone and original array share the reifier';
}
