use v6;
use Test;

# Binding a container argument to a parameter must read the element type
# from the VALUE's embedded metadata, not from the name-keyed constraint
# store: that store is scope-blind, so a method's own `my Field @f`
# declaration leaves a global "@f" entry behind, and a caller's UNTYPED
# `@f` passed to any method afterwards got retyped to Array[Field]
# (Text::CSV 46_eol_si: `.raku` rendered `Array[CSV::Field].new(...)`).

plan 4;

class Field { has $.x }

class M {
    method poison () {
        # registers a bare-name "@f" -> Field constraint in the method frame
        my Field @f = Field.new(x => 1),;
        @f.elems;
    }
    method take (@f) { @f.elems }
}

my $m = M.new;

for ("p", "q") -> $tag {
    my @f = ("a$tag", "b");
    $m.take(@f);
    is @f.raku, qq/["a$tag", "b"]/,
        "untyped \@f stays plain after method binding (iteration $tag)";
    $m.poison;
}

# The legitimate propagation still works: a TYPED caller array's element
# type reaches an untyped parameter via the value's embedded metadata.
my Int @typed = 1, 2, 3;
sub of-it (@x) { @x.of }
is of-it(@typed), Int, "typed caller array propagates .of through binding";
is @typed.raku, "Array[Int].new(1, 2, 3)", "typed array keeps its own repr";
