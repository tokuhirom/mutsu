use Test;

# An anonymous optional parameter (`$?`, `@?`, `%?`) takes the ordinary
# parameter tail -- `is` traits, a `where` post-constraint, a default -- just
# like a named one. Digest::xxHash ends a multi-line signature with
# `$? where { $*KERNEL.bits == 64 }`; the unconsumed `where` used to fail the
# whole signature at its closing paren.

plan 7;

sub gated(Int $x, $? where { True }) { $x }
is gated(7), 7, 'an anonymous optional with a where clause parses and binds';

sub checked($? where { $_ ~~ Int }) { 'ok' }
is checked(5), 'ok', 'the where clause runs against a passed value';
dies-ok { checked('nope') }, 'and rejects a value that fails it';

sub defaulted($? = 7) { 'ran' }
is defaulted(), 'ran', 'an anonymous optional may carry a default';

sub arrayish(@? where { True }) { 'ran' }
is arrayish(), 'ran', 'the @? form takes the same tail';

sub hashish(%? where { True }) { 'ran' }
is hashish(), 'ran', 'and so does %?';

# An anonymous optional invocant is still just an invocant: the `:` marker
# belongs to the parameter list, not to the tail.
class Inv { method m($?: |) { 'method' } }
is Inv.m, 'method', 'an anonymous optional invocant still parses';
