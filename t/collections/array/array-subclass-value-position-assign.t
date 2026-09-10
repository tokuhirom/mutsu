use v6;
use Test;

# An `is Array`/`is List` subclass instance is Positional, so an `@`-assignment
# distributes its ELEMENTS. mutsu had that rule on the statement-position store
# only, so the SAME assignment stored the instance whole when written where its
# result is consumed. And a SCALAR-held instance must not decompose at all --
# the itemization that keeps `my @h = $c` a single element was missing for
# instances (they have no itemized container kind of their own, exactly like
# Set/Bag/Mix and Range).

plan 14;

class SA is Array { }
class SI is Array { method iterator { (1, 2, 3, 4).iterator } }

# --- decomposition: the same assignment in both positions --------------------

{
    my @a := SA.new(3, 2, 1, 4);
    my @c = @a;
    is @c.raku, '[3, 2, 1, 4]', 'statement position distributes the elements';
    is (my @b = @a).raku, '[3, 2, 1, 4]', 'and so does value position';
    my @d;
    is (@d = @a).raku, '[3, 2, 1, 4]', '... including into an already-declared target';
    is (my @e = @a).elems, 4, 'the value-position result is the distributed array';
}

# An `iterator` override wins over the backing storage, in both positions.
{
    my @a := SI.new(9, 9);
    my @f = @a;
    is @f.raku, '[1, 2, 3, 4]', 'an iterator override drives the statement form';
    is (my @g = @a).raku, '[1, 2, 3, 4]', '... and the value form';
}

# --- itemization: a scalar-held instance stays one element -------------------

{
    my $c = SA.new(3, 2, 1, 4);
    my @h = $c;
    is @h.elems, 1, 'a scalar-held subclass instance is one element';
    is @h[0].^name, 'SA', '... and it is the instance itself';
    is (my @i = $c).elems, 1, 'the same in value position';
    my @j;
    is (@j = $c).elems, 1, '... and into an already-declared target';
}
{
    my $i = SI.new(9, 9);
    is (my @k = $i).elems, 1, 'an iterator override does not decompose from a scalar either';
}

# --- controls: the ordinary container rules are untouched --------------------

{
    my $s = [1, 2, 3];
    is (my @l = $s).elems, 1, 'a plain Array in a scalar is still one element';
    my @m = [1, 2, 3];
    is @m.elems, 3, 'an Array literal still flattens';
    my $c = SA.new(3, 2, 1, 4);
    is (my @n = @$c).elems, 4, 'an explicit @-deref still decomposes';
}
