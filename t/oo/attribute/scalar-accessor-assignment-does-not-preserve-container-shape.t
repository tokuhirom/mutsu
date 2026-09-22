use Test;

# #9005: a `$`-sigil (scalar) `is rw` accessor REBINDS on assignment -- it
# does not flow the new value INTO whatever container the attribute
# currently happens to hold, unlike `@`/`%`-sigil accessors (see
# t/oo/attribute/hash-attribute-rw-assignment.t), which do preserve the
# existing container's identity/shape. `normalize_rw_accessor_assignment`
# used to decide that preservation purely from the CURRENT value's runtime
# shape (Array/Hash), so a `$`-sigil attribute whose stored value happened
# to be an Array (e.g. from an earlier `self.x = [...]`) had every later
# assignment through the accessor -- including a `.=` coercion's own result
# -- silently coerced back into an Array. `self.x = [...]; self.x .= Hash`
# produced an Array of the Hash's pairs instead of the Hash itself.

plan 6;

class Foo {
    has $.x is rw = Whatever;
    method set {
        self.x = [a => 1, b => 2];
        self.x .= Hash;
    }
}

my $f = Foo.new;
$f.set;
is $f.x.^name, 'Hash',
    '.= through a method-call scalar accessor stores the coerced type, not the prior shape';
is $f.x<a>, 1, 'the coerced Hash keeps its entries (a)';
is $f.x<b>, 2, 'the coerced Hash keeps its entries (b)';
ok $f.x ~~ Map:D, 'the result smartmatches Map:D';

# Plain `=` (not `.=`) through the same scalar accessor must also rebind
# outright, not coerce into the previous shape.
class Bar {
    has $.y is rw = Whatever;
}
my $b = Bar.new;
$b.y = [1, 2, 3];
$b.y = { c => 3 };
is $b.y.^name, 'Hash',
    'plain assignment through a scalar accessor rebinds instead of coercing to the prior Array shape';

# An `@`/`%`-sigil accessor's container-preserving behavior (the ORIGINAL
# reason this coercion exists) must be unaffected by the sigil gate.
class Baz {
    has %.h is rw = (a => 1);
}
my $z = Baz.new;
$z.h = <x y>;
is $z.h.^name, 'Hash',
    'a %-sigil accessor still coerces a list assignment into a Hash';
