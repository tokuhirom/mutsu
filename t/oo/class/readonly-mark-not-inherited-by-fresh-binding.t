use v6;
use lib 't/lib';
use Test;

# `my @names is List` makes that binding immutable, but only that one: a
# later fresh `@names` -- a parameter or a `my` declaration in another
# scope -- is writable. The readonly marks are keyed by name, so a mark
# left by a role, class or module body used to leak into them (SBOM's role
# body broke SBOM::enums' `sub EXPORT(*@names) { @names ||= ... }`).

use ReadonlyListModule;

plan 7;

role RM { my @names is List = <a b>; method names() { @names } }
class Q does RM { }

sub slurpy(*@names) { @names ||= (1, 2); @names }
is-deeply slurpy(), [1, 2], 'a slurpy @param after a role body';

sub positional(@names) { @names = 5; @names }
is-deeply positional([0]), [5], 'a positional @param';

sub declared() { my @names; @names = 7; @names }
is-deeply declared(), [7], 'a my @names in a routine';

class C { my %h is Map = a => 1; method h() { %h } }
sub hash-param(%h) { %h = b => 2; %h }
is-deeply hash-param({}), {b => 2}, 'a %param after a class body is Map';

throws-like { assign-module-names() }, X::Assignment::RO,
    "the module's own binding stays immutable in its routines";

my @l is List = 1, 2;
throws-like { @l = 3 }, X::Assignment::RO, 'a mainline is List binding stays immutable';
is-deeply Q.names, ('a', 'b'), "the role body's own binding is unchanged";
