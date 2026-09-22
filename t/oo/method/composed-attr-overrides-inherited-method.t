use Test;

# A role's public attribute accessor is an ordinary method of the composing
# class, and it wins over a method of the same name inherited from a PARENT
# class -- the same "subclass overrides parent" rule that applies to any
# other method. `resolve_user_method_or_accessor` documents this precedence,
# but its per-MRO-level `has_attr` check only ever saw an accessor for an
# attribute declared directly in `class_def.attributes` when the class body
# had run at least one statement: `sync_accessor_entries` (which re-derives
# the accessor column from `class_def.attributes`) was called after every
# body statement, but never once, unconditionally, at the end of class
# registration -- so a class whose ONLY attribute source is header-level
# role composition (`class Bar is Foo does R {}`, an empty body) published
# its final `class_def.attributes` into the registry without ever syncing
# the accessor column, and the composed role's `greet` accessor was
# invisible to method resolution. Method lookup then fell through to the
# ancestor Foo's `method greet`, instead of stopping at Bar's own level as
# `raku` does. Reduced from `App::six-pm::SixPM`'s `does role :: { has
# $.get-project-name is rw = ...; }` runtime mixin (same underlying
# `register_class_decl` path), which showed the same collapse.

plan 4;

class Foo {
    method greet { "class-method" }
}
role R {
    has $.greet is rw = "role-attr";
}
class Bar is Foo does R { }

is Bar.new.greet, "role-attr",
    'a header-composed role attribute accessor wins over an inherited parent method';

# The runtime `does` mixin path (`Interpreter::does_rebless_instance`)
# reblesses into the very same kind of synthesized class and must agree.
class Baz {
    method greet { "class-method" }
}
my $b = Baz.new;
$b does role :: {
    has $.greet is rw = "role-attr";
};
is $b.greet, "role-attr",
    'a runtime `does` role attribute accessor wins over an inherited/own-class method';

# An attribute with no default still resolves through the constructor.
role R2 {
    has $.greet;
}
class Qux is Foo does R2 { }
is Qux.new(greet => "ctor-value").greet, "ctor-value",
    'a header-composed role attribute with no default still wins over the inherited method';

# The class's OWN explicit method still wins over a role it composes
# (class entities beat role entities at the SAME level).
role R3 {
    has $.greet is rw = "role-attr";
}
class Own is Foo does R3 {
    method greet { "own-method" }
}
is Own.new.greet, "own-method",
    q[a class's own explicit method still wins over a role attribute it composes];

done-testing;
