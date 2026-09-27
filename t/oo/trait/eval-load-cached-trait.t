use lib 't/lib';
use Test;

plan 1;

# Configuration 0.0.11 uses `is cached` in a module loaded by Test's EVAL-based
# `use-ok`. The built-in routine trait must remain valid in that load context.
use-ok 'EvalCachedTraitFixture';
