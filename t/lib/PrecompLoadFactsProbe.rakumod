unit module PrecompLoadFactsProbe;

# A precompilation hit serves this module from its recorded load facts
# (ADR-12026 §2.1), but each construct below makes the load read the AST
# itself: a declarator-documented routine, a `state`-declaring sub, and a
# module-level block phaser. (The probe does not read `.WHY`: a module
# routine's `.WHY` is Nil today, #12037.)

our $left = 'not yet';

#| Adds one.
sub inc($x) is export { $x + 1 }

sub tally is export {
    state $n = 0;
    $n += 10
}

ENTER { $left = 'entered' }

sub probe is export {
    (inc(1), tally(), tally(), $left).join('|')
}
