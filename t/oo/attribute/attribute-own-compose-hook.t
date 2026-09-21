use Test;

plan 3;

# The `Attribute::Lazy` distribution (ecosystem lock #7884) defines a
# `will lazy { ... }` trait whose handler does `$attr does Builder[$block]`
# with NO `$class.HOW` mixin at all -- unlike every other compose-hook shape
# in this directory (AttrX::Lazy-style: a role mixed into `$class.HOW`,
# whose `compose` inspects/edits the class). Real Rakudo's `Attribute` type
# has a native no-op `compose(Mu $package)` that every attribute goes
# through during class composition (verified against `raku`:
# `Foo.^attributes[0].can('compose')` is true even with no trait applied at
# all); a role mixed straight into the attribute polymorphically overrides
# it. mutsu used to defer a `compose` hook's call ONLY when it lived on
# `$class.HOW` (#8845) -- an attribute-own `compose` method was silently
# never called, so `Builder.compose`'s
# `$package.^method_table{$meth-name}.wrap(...)` never ran and the lazy
# accessor never installed.

my role Builder[Callable $block] {
    method compose(Mu $package) {
        callsame;
        my $attr = self;
        if $attr.has_accessor {
            my $meth-name = self.name.substr(2);
            $package.^method_table{$meth-name}.wrap(-> $self {
                if not $attr.get_value($self).defined {
                    $attr.set_value($self, $block($self));
                }
                callsame;
            });
        }
    }
}

multi sub trait_mod:<will>(Attribute:D $attr, Callable $block, :$lazy!) is export(:DEFAULT) {
    $attr does Builder[$block];
}

class TestPoodle {
    has $.foo will lazy { "foo" };
    has $.something is rw = "unset";
    has $.booble will lazy { "beep" ~ $_.something };
}

is TestPoodle.new.foo, "foo",
    'a compose hook mixed directly into the attribute (no $class.HOW) installs a lazy accessor';
is TestPoodle.new(foo => "boom").foo, "boom",
    "a value supplied to the constructor is not overwritten by the lazy block";

my $a = TestPoodle.new;
$a.something = 'bloop';
is $a.booble, "beepbloop",
    "the lazy block sees the instance passed to it and is invoked only once per instance";
