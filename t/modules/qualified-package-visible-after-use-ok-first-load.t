use v6;
use Test;

# A module's declared package is not derivable from the name it is `use`d by:
# `Acme/Cow.rakumod` in the Acme::Cow distribution says `unit module Cow;`, so
# its classes are `Cow::cow`, `Cow::basic`, ... The #7797 visibility gate only
# lets a compunit name `Pkg::thing` when it `use`d `Pkg` itself, and the grant
# that records it is computed during the module's load (`granted_packages` in
# src/runtime/run_modules.rs) -- it covers the declared `unit module` package,
# not just the module's own name.
#
# A re-`use` of an already-loaded module never re-runs that load, so it could
# not recompute the grant and only ever recorded the module's own name. That
# is invisible while the two match, and fatal when they do not: `Test`'s
# `use-ok` loads the module first from inside an EVAL, so the script's own
# `use` is the already-loaded no-op, and `Cow::cow` was never granted to the
# script at all -- "Could not find symbol 'Cow::cow'". The grant is now
# recorded per module at first load and replayed for later importers.
#
# From the Acme::Cow 0.2 distribution's t/01-tests.rakutest (ecosystem sweep).

plan 3;

use lib 't/lib/AcmeCowShape';

# The first load happens inside use-ok's EVAL, exactly as the distribution's
# own test file does it.
use-ok 'Acme::Cowish';

use Acme::Cowish;

ok CowNS::cow.new, 'a declared-package class is reachable after a use-ok first load';
is CowNS::cow.new.who, 'cow', 'and it is the real class, with its own methods';
