use Test;

plan 7;

# An attributive `@` parameter assigns its argument into the attribute's Array
# (`@!x = |$arg`), so the elements are fresh writable containers whatever the
# argument was (#12741).
class P { has $.name; has $.value }

class I {
    has @.env is built(False);
    submethod TWEAK(:@!env) {
        for @!env { $_ = P.new(|$_) if $_ ~~ Associative }
    }
}

is I.new(env => (Map.new((:name("Foo"), :value("Bar"))),)).env.raku,
    '[P.new(name => "Foo", value => "Bar")]',
    'a List argument is writable in TWEAK';
is I.new(env => [{name => "Foo", value => "Bar"}, "x"]).env.raku,
    '[P.new(name => "Foo", value => "Bar"), "x"]', 'an Array argument still is';

class K {
    has @.env is built(False);
    submethod TWEAK(:@!env) { for @!env { $_ = 5 } }
}
is-deeply K.new(env => (1, 2)).env, [5, 5], 'for-loop assignment over a List argument';

class J {
    has @.env is built(False);
    submethod TWEAK(:@!env) { @!env[0] = 9 }
}
is-deeply J.new(env => (1, 2)).env, [9, 2], 'element assignment over a List argument';

class S {
    has @.env;
    submethod BUILD(:@!env) { for @!env { $_ = $_ * 2 } }
}
is-deeply S.new(env => (1, 2, 3)).env, [2, 4, 6], 'the same through BUILD';

class T {
    has @.items;
    submethod BUILD(*%args, :@!items) { for @!items { $_ = "x$_" } }
}
is-deeply T.new(items => (1, 2), junk => 1).items, ["x1", "x2"], 'BUILD with a slurpy hash beside it';

class E {
    has @.items;
    submethod BUILD(:@!items) { }
}
is-deeply E.new(items => ()).items, [], 'an empty List initialises an empty Array';
