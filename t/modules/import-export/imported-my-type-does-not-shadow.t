use v6;
use lib 't/lib';
use Test;

use Issue8120LeakMy;

plan 5;

ok ::('State') ~~ Failure,
    'a module body my class does not leak its bare name to the importer';
ok ::('Issue8120LeakMy::State') ~~ Failure,
    'a module body my class does not leak its qualified name either';
is leak-state().who, 'private',
    'the declaring module can still use its private class';

module Issue8120Consumer {
    my class State {
        method who() { 'consumer' }
    }

    our sub consumer-state() {
        State.new.who ~ '/' ~ State.^name
    }
}

is Issue8120Consumer::consumer-state(), 'consumer/Issue8120Consumer::State',
    'the importer own same-named my class wins over the imported one';
is Issue8120Consumer::consumer-state(), 'consumer/Issue8120Consumer::State',
    'the winning local type remains stable on repeated use';
