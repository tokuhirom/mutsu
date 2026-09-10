use Test;
plan 7;

# `Foo.new(1,2,3)` for a class `is Array` with NO user-defined `new` goes
# through the generic default-constructor path (dispatch_new's positional-arg
# handling), which used to stash the elements under a stray attribute key
# (`__array_items`) that none of the Array-delegation methods (elems,
# AT-POS, push, ...) actually read -- they all read `__mutsu_array_storage`
# (the same key the `nextwith(|@values)` bless-time path uses). So
# `Foo.new(1,2,3).elems` silently came back 0.
class Foo is Array {
    method AT-POS($index) { nextwith $index.round }
}

my $f = Foo.new(1, 2, 3);
is $f.^name, 'Foo', 'no-args-defined new still tags the subclass';
is $f.elems, 3, 'positional args populate the backing storage';
is $f[0], 1, 'AT-POS reads the backing storage';
is $f[1], 2, 'AT-POS reads the backing storage (2)';

# A custom Positional override calling nextwith reaches the native array
# base, even though the class has no OTHER user candidate in the MRO for
# AT-POS -- a single (non-multi, non-wrapped) compiled method pushes no
# `method_dispatch_stack` frame, so this used to fall through the
# exhausted-MRO check straight to Nil. Called directly (not via subscript
# syntax, which truncates the index to Int before AT-POS ever sees it --
# confirmed against raku: `$f[1.5]` calls AT-POS with 1, not 1.5).
is $f.AT-POS(1.5), 3, 'nextwith from a Positional override reaches the native Array base';

# Subscript syntax on an instance truncates a fractional index to Int before
# dispatching to AT-POS, exactly like a plain Array -- it used to fall
# through every match arm straight to Nil for a non-Int index.
is $f[1.5], 2, 'a fractional subscript on an instance truncates like a plain Array';

my @g is Foo = <a b c>;
is @g.^name, 'Foo', 'the "is" trait on an @-sigil variable blesses as the user Array subclass';
