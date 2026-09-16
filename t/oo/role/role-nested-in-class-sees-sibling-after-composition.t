use Test;

plan 4;

# https://github.com/tokuhirom/mutsu/issues/8565
#
# A role nested inside a class may refer to a bareword type that is a
# SIBLING of the role -- also nested in the same enclosing class, but not
# inside the role itself. Rakudo anchors a role method's lexical visibility
# at the role's own declaration site, so this keeps resolving to the
# enclosing class's nested type even after the role is composed into a
# wholly unrelated consumer class. mutsu used to resolve the bare name
# against the CONSUMING class instead, throwing X::Undeclared::Symbols.

class Outer {
    class Inner { }
    role UsesInner {
        method make() { Inner.new }
    }
}
class Consumer does Outer::UsesInner { }

is Consumer.new.make.^name, 'Outer::Inner',
    'a role method sees a sibling nested class through composition into an unrelated consumer';

# Two levels of nesting: the walk must climb past the immediate enclosing
# package (Mid) to reach the sibling (Outer::Deepest).
class DeepOuter {
    class Deepest { }
    class Mid {
        role UsesDeepest {
            method make() { Deepest.new }
        }
    }
}
class DeepConsumer does DeepOuter::Mid::UsesDeepest { }

is DeepConsumer.new.make.^name, 'DeepOuter::Deepest',
    'the walk climbs past an intermediate nesting level to find the sibling';

# The role only lends ITS OWN lexical scope chain -- a method the consumer
# declares itself must not gain access to the role's enclosing sibling types.
class Other {
    class Inner { method who() { 'other-inner' } }
}
class ConsumerWithOwnMethod does Outer::UsesInner {
    method own() { Other::Inner.new.who }
}

is ConsumerWithOwnMethod.new.make.^name, 'Outer::Inner',
    'the composed method still resolves the role-side sibling';
is ConsumerWithOwnMethod.new.own, 'other-inner',
    'a method the consumer declares itself resolves its own bareword normally';

done-testing;
