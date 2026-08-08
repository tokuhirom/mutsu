use Test;
use MONKEY-TYPING;

# ADR-0019 D3-5 follow-up: `handles` on an augment-declared method now
# synthesizes forwarder methods, matching the class/role walkers. Verified
# against raku.

plan 2;

# Plain `handles 'name'`
{
    class Forward1 { }
    augment class Forward1 {
        method inner() handles 'uc' { 'hello' }
    }
    is Forward1.new.uc, 'HELLO', 'handles Name forwards to the target method';
}

# Renamed `handles (exposed => 'target')`
{
    class Forward2 { }
    augment class Forward2 {
        method inner() handles (up => 'uc') { 'hello' }
    }
    is Forward2.new.up, 'HELLO', 'handles Rename forwards under the exposed name';
}

# NOTE: `handles *` (Wildcard) is deliberately not covered here — it hits a
# separate, pre-existing bug shared by the class/role walkers too (a
# built-in Cool method like `.uc` wins over wildcard delegation instead of
# the reverse), not an augment-specific drift. See
# todo/tickets/wildcard-handles-loses-to-builtin-cool-methods.md.
