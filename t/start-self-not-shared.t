use v6;
use Test;

plan 3;

# `self` (and ?CLASS/?ROLE) are per-invocation bindings, never shared
# variables: an earlier `start` block's invocant must not leak into a later
# start block's `self` via the await-time shared-var sync. The trigger is an
# `await` INSIDE the second start block (its resume path pulled the polluted
# shared store back into env, swapping self for the previous start's
# invocant — Cro::CompositeConnector read another connector's attributes).
class First {
    has $.name = "first-inst";
    method m(--> Promise) {
        start { self.^name }
    }
}

class Second {
    has $.label = "second-inst";
    method m(--> Promise) {
        start {
            my $x = await Promise.kept("k");
            (self.^name, $.label)
        }
    }
}

is (await First.new.m), 'First', 'first start block sees its own self';
my ($cls, $label) = await Second.new.m;
is $cls, 'Second', 'second start block still sees its own self after await';
is $label, 'second-inst', 'attribute reads resolve against the right instance';
