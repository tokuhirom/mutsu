use v6.e.PREVIEW;
use lib 't/lib-use-cycle-clique';
use Test;

# Modules that load each other all round (a `use` one way, a `require` in a
# routine the other way) are scanned once per cycle context, not once per path
# through the cycle: the number of paths of nine such modules is exponential, so a
# scan that re-parses a module for every path never finishes (#13370,
# Qwiratry::Walker::Factory took 87 s to load).

plan 3;

use UseCycle1;
use UseCycle9;

is marker1(), 1, 'the entry module of the cycle loads';
is UseCycle1::name(), 'UseCycle1', 'its own routine is callable by qualified name';
is load-earlier(), 9, 'the last module requires every earlier one at run time';
