use Test;

plan 8;

class Foo {
    has $!bar is built;
    has $.baz is built(False);
    has $!qux is built(:bind);
    has $.z is built(:bind);

    method bar { $!bar }
    method qux { $!qux }
}

# `is built` assigns into a Scalar: an Array is itemized.
is Foo.new(:bar[]).bar.raku, '$[]', 'is built itemizes the Array';
is Foo.new(:baz[]).baz.raku, 'Any', 'is built(False) ignores the argument';
# `is built(:bind)` binds the argument itself.
is Foo.new(:qux[]).qux.raku, '[]', 'is built(:bind) keeps the Array un-itemized';
is Foo.new(:z[1, 2]).z.raku, '[1, 2]', 'public bound attribute';
my @a = 1, 2;
is Foo.new(:qux(@a)).qux.raku, '[1, 2]', 'bound @-variable';
my %h = a => 1;
is Foo.new(:qux(%h)).qux.raku, '{:a(1)}', 'bound %-variable';
is Foo.new(:qux(5)).qux, 5, 'a plain value is stored';
my Foo:D $foo .= new: :bar[], :baz[], :qux[];
is $foo.qux.raku, '[]', 'the doc example (Type/Attribute)';
