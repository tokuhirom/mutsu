use v6;
use lib 't/lib';
use Test;

# Regression: a `multi infix:<**> is export`ed from a module supersedes the
# core operator, so a literal-only expression like `64 ** ⅓` must dispatch to
# the module's override at runtime rather than being constant-folded against
# the core `**` at compile time. mutsu loads modules at runtime, so the
# compiler must treat a module `use` conservatively and disable literal
# operator folding for the unit (ADR-0006 known gap, fixed for module imports).

use RatPowerFixture;

plan 4;

# The headline case: both operands are literals, so the compile-time folder
# would evaluate this eagerly if it did not know about the imported override.
my $r = 64 ** ⅓;
isa-ok $r, Int, '64 ** ⅓ dispatches to module override -> Int (not folded Num)';
is $r, 4, '64 ** ⅓ == 4 via the module override';

# A non-ExpRat exponent falls through to the core operator (Num result).
my $s = 60 ** ⅓;
isa-ok $s, Num, '60 ** ⅓ uses the core operator (Num)';

# Ordinary (non-operator) constant folding is unaffected by the import.
constant SECONDS = 60 * 60;
is SECONDS, 3600, 'plain constant folding still works alongside the import';
