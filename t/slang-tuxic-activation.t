use v6;
use lib 't/lib';
use Test;
use Slang::Tuxic;

# End-to-end slang activation (ADR-0026): `use Slang::Tuxic` executes the
# bundled Slangify + Slang::Tuxic verbatim at parse time; the roles' rule
# overrides flip the parser's spaced-call / spaced-methodop modes for the
# rest of this compilation unit — and only this unit.

plan 8;

sub foo($a, $b) { $a * $b }
is foo (3, 5), 15, 'spaced call passes the paren contents as an arg list';
is foo(3, 5), 15, 'unspaced call unchanged';
is 42.fmt ('-%d-'), '-42-', 'spaced methodop is a method call';

sub named(:$x, :$y) { "$x/$y" }
is named (x => 1, y => 2), '1/2', 'named args bind as named through the spaced call';

use TuxicScoped;
is scoped-spaced-add(3, 4), 7, 'a module using the slang parses in Tuxic mode';

# Slang state is lexical to the compilation unit: an EVAL string is its own
# unit and parses in the stock grammar.
dies-ok { EVAL q[my sub m2($a, $b) { $a }; m2 (1, 2)] },
    'EVAL string parses in the stock grammar (listop call, arity error)';
dies-ok { EVAL q[42.fmt ('-%d-')] },
    'EVAL string keeps the stock no-space-before-paren methodop error';

# The exclusion list: control keywords stay control flow under Tuxic mode.
my $kw = do if (1) { 'kw' } else { 'no' };
is $kw, 'kw', 'if (…) stays an if under Tuxic mode';
