use Test;

plan 3;

# Assigning through a private `is rw` method that returns a Proxy runs STORE
# alone; raku FETCHes only when something reads the assignment's value (#12322).
my @log;
class P {
    method !pm($n) is rw {
        Proxy.new(
            FETCH => sub ($) { @log.push("FETCH $n"); 1 },
            STORE => sub ($, $v) { @log.push("STORE $n $v") },
        );
    }
    method t { self!pm("priv") = 1; 0 }
}

is P.new.t, 0, 'statement-level private-method assignment runs';
is @log.join(','), 'STORE priv 1', 'only STORE fires, no FETCH';
is @log.elems, 1, 'exactly one callback ran';
