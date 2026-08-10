use Test;

plan 2;

# A *%options slurpy hash parameter read inside a nested `start {}` block
# must resolve to THIS call's own arguments, even across purely sequential
# (non-overlapping, same-thread) calls to the same routine. The mask that
# marks a slurpy parameter as call-local must survive into the spawned
# thread's own body, independent of whether the synchronous call that
# created it has already returned by the time that body runs.
{
    sub connect(*%options) {
        start %options<prepend>;
    }

    my $h1 = await connect(prepend => 'un');
    my $h2 = await connect(prepend => 'in');
    is $h1, 'un', 'first sequential call reads its own slurpy hash param';
    is $h2, 'in', 'second sequential call does not read back the first call\'s value';
}

done-testing;
