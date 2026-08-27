use v6;
use Test;

# A user-defined `method ^parameterize` makes an otherwise non-parametric
# class/grammar parametric: `Type[...]` is a call to the metaclass's
# `parameterize`, per `Language/mop.rakudoc`'s "parametric" archetype example.
#
# Two things are pinned here:
#   1. `method ^parameterize` is actually dispatched by `Type[...]`
#      (it used to be ignored, leaving X::NotParametric).
#   2. A metamethod's `self` is the type's HOW, not the type object, so
#      `self.name($this)` works (Rakudo binds the metaclass).
#
# The grammar case also pins the fix for an unbounded
# `is_container_subclass` recursion: `grammar Bot::Grammar` has parent
# `Grammar`, and the registry's short-name fallback resolved that parent name
# back to `Bot::Grammar` itself, so the parent-chain walk recursed forever and
# overflowed the stack (exit 134) on `Bot::Grammar[...]`.

plan 8;

class Plain {
    method ^parameterize(::?CLASS:U $this is raw, +roles) {
        my Str:D $name   = self.name: $this;
        my Mu    $mixin := $this.^mixin: |roles;
        $mixin.^set_name: [~] $name, '[', roles.map(*.^name).join(','), ']';
        $mixin
    }
}

role Loud { method speak() { 'LOUD' } }

my $loud = Plain[Loud];
is $loud.^name, 'Plain[Loud]', 'user ^parameterize renames the parameterized type';
is $loud.new.speak, 'LOUD', 'the mixed-in role method is callable';
ok Plain.new !~~ Loud, 'the base type is untouched by the parameterization';

# `self` inside a metamethod is the metaobject, not the type object.
class Meta {
    method ^who($this) { self.name($this) }
}
is Meta.^who, 'Meta', 'metamethod self is the HOW, so self.name($type) works';

# The stack-overflow case: a grammar whose short name collides with its own
# parent (`Grammar`), parameterized with a role that adds a `topic` candidate.
grammar Bot::Grammar {
    token TOP { <topic> || .+ }

    proto token topic {*}
    multi token topic:sym<command> { '$' <!ws>+ }

    method ^parameterize(::?CLASS:U $this is raw, +roles) {
        my Str:D $name   = self.name: $this;
        my Mu    $mixin := $this.^mixin: |roles;
        $mixin.^set_name: [~] $name, '[', roles.map(*.^name).join(','), ']';
        $mixin
    }
}

role Greetings[Str:D $name] {
    multi token topic:sym<greeting> { ^ [ 'hi' | 'hello' | 'hey' | 'sup' ] <.ws> $name }
}

my constant GreetBot = Bot::Grammar[Greetings['GreetBot']];
ok GreetBot.^name.starts-with('Bot::Grammar['),
    'parameterizing a `X::Grammar` grammar does not blow the stack';
GreetBot.parse: 'sup GreetBot';
is ~$/, 'sup GreetBot', 'the role-supplied token candidate participates in the parse';

# A `X::<builtin>` class name must not make the plain parent walk recurse
# either: these queries all walk the same registry parent chain.
class Zoo::Cool is Cool { }
ok Zoo::Cool.new.defined, 'a class named after its own builtin parent is constructible';

# Parameterizing a class WITHOUT a ^parameterize metamethod still throws.
# (Written as a string so the failure happens at EVAL time -- Rakudo catches
# this one at compile time.)
throws-like 'class NoParam { }; NoParam[Int]', X::NotParametric,
    'a class with no ^parameterize is still not parameterizable';

done-testing;
