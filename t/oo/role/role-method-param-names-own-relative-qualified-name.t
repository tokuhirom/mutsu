use Test;

# A role with a compound name declared inside a package registers under the
# package (`module Core { role A::Item }` is `Core::A::Item`), and its own
# methods name it by the relative spelling `A::Item`. Role-method registration
# accepted only the full registered name or its last segment (`Item`) as a
# self-reference, so `A::Item` / `A::Item:U` was rejected as "Invalid typename
# 'A::Item:U' in parameter declaration." — which stopped every module of
# Intl::CLDR (`unit module Core; role CLDR::Item { ... CLDR::Item:U: ... }`)
# from loading.

plan 7;

module Core {
    role A::Item {
        multi method AT-KEY(A::Item:U: $k) { "type:$k" }
        multi method AT-KEY(A::Item:D: $k) { "inst:$k" }
        method same(A::Item $other) { $other.^name }
        method defined-only(A::Item:D $other) { 'defined' }
    }
    class Thing does A::Item { }
    our sub thing() { Thing }
    our sub takes(A::Item:D $x) { 'sub:' ~ $x.^name }
}

my \T = Core::thing();
is T<k>, 'type:k', 'a `A::Item:U` invocant candidate dispatches on the type object';
is T.new<k>, 'inst:k', 'a `A::Item:D` invocant candidate dispatches on an instance';
like T.same(T.new), /Thing/, 'a bare relative self-name parameter accepts a composing class';
is T.defined-only(T.new), 'defined', 'a `A::Item:D` parameter accepts an instance';
dies-ok { T.defined-only(T) }, 'the smiley still constrains the relative self-name';

like Core::takes(T.new), /^'sub:' .* Thing/,
    'a sub in the declaring package binds the relative compound role name';

# A genuinely unknown qualified name is still rejected at declaration.
throws-like 'module M { role A::Item { method m(B::Item $x) { } } }',
    X::Parameter::InvalidType, 'an unrelated qualified name is still invalid';
