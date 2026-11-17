use Test;

# A second module that `use`s an operator-re-exporting `sub EXPORT` module
# (Qwiratry::Query::Slang) must still see the operators. The runtime's
# canonical export scan of the slang ran with no module search paths, so its
# `need` of the operator module went unresolved and the operator-less scan
# was cached for the next importer (Qwiratry::Test::Location after
# Qwiratry::Test::Format, t/00-use.rakutest).
#
# EhCycMid once `use`d EhCycSlang from a sub body to truncate the slang's scan
# through a module cycle; rakudo rejects that cycle ("Circular module loading
# detected"), and so does mutsu since a `use` loads at compile time
# (ADR-17558), so the fixture no longer closes it. Checked against rakudo.

plan 2;

use lib 't/lib';
use EhCycA;
use EhCycB;

is a, 1, 'the first importer of the slang loads';
is b, 'parse(x,y)', 'the second importer parses and calls the re-exported infix';
