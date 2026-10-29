use Test;

# A second module that `use`s an operator-re-exporting `sub EXPORT` module
# (Qwiratry::Query::Slang) must still see the operators. The runtime's
# canonical export scan of the slang ran with no module search paths, so its
# `need` of the operator module went unresolved and the operator-less scan
# was cached for the next importer (Qwiratry::Test::Location after
# Qwiratry::Test::Format, t/00-use.rakutest).
#
# EhCycMid's `use` sits in a sub body: the scan follows it, the runtime does
# not, so the slang scan is truncated by the recursion guard and is first
# cached by the runtime's canonical scan instead.

plan 2;

use lib 't/lib';
use EhCycA;
use EhCycB;

is a, 1, 'the first importer of the slang loads';
is b, 'parse(x,y)', 'the second importer parses and calls the re-exported infix';
