use lib 't/lib';
use Test;

# A closure written in a module and run on a worker thread resolves its calls
# through the module's routine tables. A block of that routine declares inner
# subs (`find-factor`, and a `split-off` shadowing the module's own) while it
# runs on the worker; the base-name key index the worker kept over its OWN
# tables must not be read over the module's ones, and vice versa. It was: the
# worker named `find-factor` as registered after switching back, and the call
# died with "Unknown function: find-factor" (Math::Sequences t/OEIS.rakutest).

use ThreadInnerSubFactors;

plan 2;

is-deeply (await start { @ThreadInnerSubFactors::COUNTS[^10] }),
    (0, 0, 1, 1, 2, 1, 2, 1, 3, 2),
    'a lazy module array whose routine declares inner subs, read from start';

is-deeply (await start { @ThreadInnerSubFactors::COUNTS[^14] }),
    (0, 0, 1, 1, 2, 1, 2, 1, 3, 2, 2, 1, 3, 1),
    'reading it again from another start block';
