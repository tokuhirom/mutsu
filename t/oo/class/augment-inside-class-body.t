use Test;
use MONKEY-TYPING;

# An `augment class` written inside another class's body declares its methods
# on the augmented type, not on the enclosing class. The nested-method hoisting
# used to move them out into the enclosing class (Int::polydiv's
# `unit class Int::polydiv; augment class Int { method polydiv ... }`).

plan 5;

class Holder {
    augment class Int {
        method twice { self * 2 }
    }
    method own { 'own' }
}

is 21.twice, 42, 'the augmented type gets the method';
nok Holder.^can('twice'), 'the enclosing class does not';
is Holder.own, 'own', "the enclosing class keeps its own methods";

class Outer {
    augment class Str {
        method shout { self.uc ~ '!' }
    }
}
is 'hi'.shout, 'HI!', 'works for another core type';
nok Outer.^can('shout'), 'and stays off the enclosing class';
