use Test;

plan 3;

# #8845: AttrX::Lazy's own natural style declares the lazy attribute BEFORE
# its private builder method:
#
#   class Sample {
#       has $.attribute is lazy;
#       method !build_attribute() { 42 }
#   }
#
# `is lazy`'s `trait_mod:<is>` handler mixes a role into `$class.HOW` whose
# `compose` method checks `type.^private_method_table` for the builder. mutsu
# used to run that `compose` hook INLINE, as soon as the `has` statement's op
# ran -- before any later class-body statement (here, `method
# !build_attribute`) had registered, so the check always failed. `raku` runs
# every class-body method's declaration before any attribute-trait side
# effect (verified directly against `raku`), regardless of source order.
# Reordering the two statements always worked in mutsu, which is what
# isolates this as a source-order bug rather than a logic bug in the
# accessor itself.

my role ChecksPrivateBuilder {
    method compose(Mu \type) {
        state $saw-builder;
        $saw-builder = type.^private_method_table<build_it>:exists;
        type.^add_method('saw-builder-in-compose', method (Mu:D:) { $saw-builder });
        callsame;
    }
}
multi trait_mod:<is>(Attribute:D $attr, :$checks-private-builder!) {
    my $class := $attr.package;
    unless $class.HOW ~~ ChecksPrivateBuilder {
        $class.HOW does ChecksPrivateBuilder;
    }
}

class Sample {
    # The attribute's trait fires composition BEFORE this class body has
    # reached the `method !build_it` declaration below.
    has $.attribute is checks-private-builder;
    method !build_it() { 42 }
}

ok Sample.new.saw-builder-in-compose,
    'a compose hook triggered by an attribute trait sees a method declared LATER in the class body';

# A two-attribute class exercises the same guard AttrX::Lazy relies on
# (`unless $class.HOW ~~ Role { ... }`): compose fires once, on the first
# `has` line, and by then BOTH later builder methods must already be visible.
my role ChecksTwoBuilders {
    method compose(Mu \type) {
        state $saw-both;
        $saw-both = (type.^private_method_table<build_a>:exists)
            && (type.^private_method_table<build_b>:exists);
        type.^add_method('saw-both-in-compose', method (Mu:D:) { $saw-both });
        callsame;
    }
}
multi trait_mod:<is>(Attribute:D $attr, :$checks-two-builders!) {
    my $class := $attr.package;
    unless $class.HOW ~~ ChecksTwoBuilders {
        $class.HOW does ChecksTwoBuilders;
    }
}

class TwoAttrs {
    has $.a is checks-two-builders;
    has $.b is checks-two-builders;
    method !build_a() { 1 }
    method !build_b() { 2 }
}

ok TwoAttrs.new.saw-both-in-compose,
    'a single deferred compose call (fired once, on the first has-line) sees every builder declared anywhere in the body';

# The full real-world shape (#8845's own repro): AttrX::Lazy-style, using the
# actual accessor-installation pattern rather than a bespoke observation
# method, confirming the accessor genuinely runs the later-declared builder.
my role LazyAttribute {
    has $.base-name = self.name.substr(2);
    has $.builder is rw = "build_" ~ self.base-name;
}
my role LazyAttributeContainerHOW {
    method compose(Mu \type) {
        for type.^attributes.grep(LazyAttribute) -> $attr {
            next unless $attr.builder ~~ type.^private_method_table;
            type.^add_method($attr.base-name, method (Mu:D:) {
                self.^private_method_table{$attr.builder}(self);
            });
        }
        callsame;
    }
}
multi trait_mod:<is>(Attribute:D $attr, :$lazy8845!) {
    my $class := $attr.package;
    $attr does LazyAttribute;
    unless $class.HOW ~~ LazyAttributeContainerHOW {
        $class.HOW does LazyAttributeContainerHOW;
    }
}

class LazySample {
    has $.attribute is lazy8845;
    method !build_attribute() { 42 }
}

is LazySample.new.attribute, 42,
    'the real AttrX::Lazy shape installs and runs a lazy accessor for a builder declared after the has-line';
