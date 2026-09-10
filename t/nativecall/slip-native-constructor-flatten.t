use Test;

# ADR-0054 S4: a `Slip` VALUE (not `|EXPR` syntax) reaching a call as an
# ordinary argument is one argument at the CALL level (S1), but it still
# flattens at BIND time into a genuinely slurpy-positional parameter,
# independent of call-site syntax. mutsu's method-call paths (Slice 3) no
# longer blindly flatten every Slip-shaped argument at the call op, which
# uncovered a family of NATIVE constructors (List/Array-subclass `.new`, the
# `bless` fallback for `nextwith(|@values)`-style subclassing, and
# Set/Bag/Mix `.new`) that had been relying on that old blind flattening to
# build their own slurpy element list, instead of flattening the Slip
# themselves. This mirrors the identical fix already applied to
# `map`/`grep`'s slurpy `+@list` parameter in
# `src/runtime/builtins_collection_mapgrep.rs` (see the ADR's Slice 2 notes).
#
# This exact shape broke `Cro::HTTP::Body`'s `Cro::HTTP::MultiValue.new(
# $existing.Slip, $p.value)` (a `MultiValue is List` class with no `new` of
# its own) -- roast/battery regression on `http-request-parser.rakutest`.

# --- List/Array-subclass `.new` (Cro::HTTP::MultiValue's own shape) ---
class MyList is List { }

{
    my $existing = MyList.new(1, 3);
    # `.Array` normalizes away the MyList subclass identity so `is-deeply`
    # compares plain element content (a MyList result stays MyList under
    # `.List`, which would otherwise fail `is-deeply` on TYPE, not content).
    is-deeply MyList.new($existing.Slip, 4).Array, (1, 3, 4).Array,
        'is List subclass .new: a plain (non-|) Slip argument flattens into the positional backing storage';
}

is MyList.new(Empty).elems, 0,
    'is List subclass .new: an empty Slip flattens to zero elements (not one array-of-Slip element)';

class MyArray is Array { }
{
    my $existing = MyArray.new(1, 3);
    is-deeply MyArray.new($existing.Slip, 4).Array, (1, 3, 4).Array,
        'is Array subclass .new: a plain (non-|) Slip argument flattens into the positional backing storage';
}

# --- Mu.new's own arity rule is untouched: a Slip is still ONE positional
# argument for a class with no positional constructor at all (S1), even an
# EMPTY one -- `Foo.new(Empty)` must still reject, exactly like raku, even
# though the same value flattens to zero elements above. ---
class Foo { has $.a = 42; }
dies-ok { Foo.new(Empty) },
    'Mu.new (non-positional class): even an EMPTY Slip counts as one rejected positional argument';
dies-ok { Foo.new((1, 2).Slip) },
    'Mu.new (non-positional class): a non-empty Slip is also a rejected positional argument';

# --- Set/Bag/Mix .new (the direct native QuantHash constructors) ---
{
    my $existing = (1, 3).Slip;
    my $bag = Bag.new($existing, 4);
    is $bag.total, 3, 'Bag.new: a plain (non-|) Slip argument flattens into the element count';
    is-deeply $bag.keys.sort(*.Int), (1, 3, 4), 'Bag.new: flattened elements are correct';

    my $set = Set.new($existing, 4);
    is-deeply $set.keys.sort(*.Int), (1, 3, 4), 'Set.new: a plain (non-|) Slip argument flattens into the elements';

    my $mix = Mix.new((1, 3.5).Slip, 4);
    is $mix.total, 3, 'Mix.new: a plain (non-|) Slip argument flattens into the element count';
}

done-testing;
