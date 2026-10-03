use Test;

# `CALLER::OUR::` is the caller's package stash, and a stash entry is the
# symbol's own container: writing through `.kv` reaches the `our` variable.
# From P5reset's `reset`.

plan 9;

our $a = 42;
our @b = 1, 2, 3;
our %c = x => 1;

sub caller-keys() { CALLER::OUR::.keys.grep(/^<[$@%]>/).sort.List }
is-deeply caller-keys(), ('$a', '%c', '@b'), 'CALLER::OUR:: lists the caller package';

sub clear(Str $sigils) {
    for CALLER::OUR::.kv -> \key, \value {
        next unless $sigils.contains(key.substr(0, 1));
        value = value ~~ Iterable || value ~~ Associative ?? Empty !! Nil;
    }
}

clear('$');
nok $a.defined, '$a cleared through CALLER::OUR::.kv';
is-deeply @b, [1, 2, 3], '@b untouched';
is-deeply %c, {x => 1}, '%c untouched';

clear('@%');
is-deeply @b, [], '@b emptied in place';
is-deeply %c, {}, '%c emptied in place';

for OUR::.kv -> \key, \value { value = 7 if key eq '$a' }
is $a, 7, 'OUR::.kv hands out the variable container';

package P {
    our $z = 1;
    our sub from-p() { CALLER::OUR::.keys.grep(/^'$'/).sort.List }
}
is-deeply P::from-p(), ('$a',), 'CALLER::OUR:: is the caller package, not the callee';
is-deeply OUR::.keys.grep(/^'$'/).sort.List, ('$a',), 'OUR:: unchanged';
