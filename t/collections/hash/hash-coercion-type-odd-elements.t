use Test;

plan 7;

# A `Hash()` coercion type calls `.Hash`, which dies with
# X::Hash::Store::OddNumber on a lone scalar or an odd-length list, instead of
# padding the last key with Any.
# From URI::Query::FromHash, whose `hash2query('')` relies on
# `CATCH { return '' when X::Hash::Store::OddNumber }; my Hash() $hash = $input`.

throws-like { my Hash() $h = ''; }, X::Hash::Store::OddNumber, 'Str into my Hash() $h';
throws-like { my Hash() $h = 42; }, X::Hash::Store::OddNumber, 'Int into my Hash() $h';
throws-like { my Hash() $h = (1, 2, 3); }, X::Hash::Store::OddNumber, 'odd-length list';

my Hash() $pairs = (a => 1, b => 2);
is-deeply $pairs, {a => 1, b => 2}, 'a list of pairs still coerces';

my Hash() $flat = (1, 2);
is-deeply $flat, {'1' => 2}, 'an even-length list still coerces';

my Hash() $empty = ();
is-deeply $empty, {}, 'an empty list coerces to an empty Hash';

sub q($input) {
    CATCH { return 'caught' when X::Hash::Store::OddNumber }
    my Hash() $hash = $input;
    $hash.elems
}
is q(''), 'caught', 'the odd-number error is catchable by a CATCH `when`';
