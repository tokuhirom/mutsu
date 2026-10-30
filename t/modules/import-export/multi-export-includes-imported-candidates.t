use Test;
use lib 't/lib';

plan 5;

# A module that imported a family's `proto` and declares its own `multi`
# exports the imported candidates too (rakudo), so re-exporting both
# modules' stashes keeps every candidate.
{
    use MultiExpBoth;
    is task('fred'), 'str fred', 'the imported Str candidate survives the merge';
    is task('x'.IO), 'io x', 'the IO candidate dispatches too';
}

{
    use MultiExpT;
    use MultiExpIO;
    is MultiExpIO::EXPORT::DEFAULT::<&task>.candidates.elems, 2,
        'the declaring module exports its own and the imported candidate';
    is MultiExpT::EXPORT::DEFAULT::<&task>.candidates.elems, 1,
        'the provider exports only its own';
    is task('y'), 'str y', 'direct imports dispatch as before';
}
