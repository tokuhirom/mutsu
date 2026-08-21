use Test;

plan 8;

# A direct `.ASSIGN-POS(...)` method call on an `is Array` subclass instance
# must mutate the backing storage, the same way subscript-syntax assignment
# (`$f[1] = 99`) and a real named `@a.ASSIGN-POS(...)` call already do.
# Regression for todo/tickets/assign-pos-direct-call-not-mutating-array-subclass-instance.md:
# the synthetic `"__mutsu_array_tmp"` binding used by the Array-subclass
# delegation fallback was never seeded into `self.env` before dispatch, so the
# identity-based write-back scan (`overwrite_array_bindings_by_identity`)
# found nothing to mutate and the call silently no-op'd.

class Foo is Array { }

{
    my $f = Foo.new(1, 2, 3);
    my $result = $f.ASSIGN-POS(1, 99);
    is $f[1], 99, 'ASSIGN-POS mutates the backing storage (subscript read)';
    is $f.AT-POS(1), 99, 'ASSIGN-POS mutates the backing storage (AT-POS read)';
    is $result, 99, 'ASSIGN-POS returns the assigned value';
    is-deeply $f.List, (1, 99, 3), 'the whole backing array reflects the mutation';
}

{
    # ASSIGN-POS beyond the current length extends the array (raku pads with
    # Any holes and warns on later stringification, which we don't assert).
    my $f = Foo.new(1, 2, 3);
    $f.ASSIGN-POS(4, 42);
    is $f.elems, 5, 'ASSIGN-POS past the end grows the backing storage';
    is $f[4], 42, 'the newly-assigned slot holds the new value';
}

{
    # DELETE-POS shares the same fallback dispatch/write-back path.
    my $f = Foo.new(1, 2, 3);
    is $f.DELETE-POS(1), 2, 'DELETE-POS returns the removed value';
    is $f[0], 1, 'DELETE-POS leaves the other elements of the backing storage intact';
}
