use v6;
use lib 't/lib';
use Test;

# End-to-end slang activation (ADR-0026): `use Piersing` (via Slangify)
# executes the bundled Slangify verbatim at parse time; the fixture role's
# `identifier`/`name` rule overrides (t/lib/Piersing.rakumod, mirroring
# Slangify's own upstream t/Piersing.rakumod fixture) let a bare identifier
# end in a trailing `?`/`!` for the rest of this compilation unit — and only
# this unit. See todo/tickets/slang-piersing-identifier-name-overrides.md.

use Piersing;

plan 6;

sub pass?(|c) { pass |c }
pass? 'a sub declared and called with a trailing ? parses under Piersing';

sub flunk!() { flunk 'unreachable' }
ok True, 'a sub declared with a trailing ! parses under Piersing (not called)';

my $ran = False;
sub mark?() { $ran = True }
mark?;
ok $ran, 'a zero-arg call with a trailing ? actually invokes the ?-suffixed sub';

# A `?`-suffixed name must not swallow the FIRST half of a doubled `??`
# compact ternary that immediately follows (no separating whitespace).
sub cond?() { True }
is (cond?()??'yes'!!'no'), 'yes', 'a compact ternary right after a ?-suffixed call still parses';

# The `?`/`!` suffix is still only ONE trailing character: `foo??` is not a
# legal identifier under Piersing, so `foo?` followed immediately by another
# `?` reads as the identifier `foo?` plus a fresh (here, dangling) `?`.
sub double?() { True }
throws-like 'sub weird??() { 1 }', Exception,
    'a double `??` suffix on a declaration is not swallowed as one identifier';

# Slang state is lexical to the compilation unit: an EVAL string is its own
# unit and parses in the stock grammar, where a bare trailing `?` is not part
# of an identifier.
dies-ok { EVAL q[sub eval-pass?(|c) { pass |c }] },
    'EVAL string parses in the stock grammar (trailing ? is not part of a name)';
