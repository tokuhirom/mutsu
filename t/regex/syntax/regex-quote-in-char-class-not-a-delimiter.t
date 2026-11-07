use v6;
use Test;

# A quote character inside a `<[...]>` class is a member, not a string
# delimiter. A `"` there made the interpolation pre-pass think the next
# single-quoted literal sat inside a double-quoted atom, so `'$B'` was
# interpolated as a variable instead of matched literally. Found with
# App::Ariza t/12-launcher (`<["\#]> '$BUNDLE_ROOT'`). Identical under raku.

plan 7;

my $B = "zz";
is 'a"$B c a"zz'.comb(/ <["]> '$B' /).raku, '("\"\$B",).Seq',
    'a single-quoted `$B` after a class holding a double quote stays literal';

my $sh = 'x "$BUNDLE_ROOT/a" y "inst#$BUNDLE_ROOT" z "$BUNDLE_ROOT" $BUNDLE_ROOT';
is +$sh.comb(/ '$BUNDLE_ROOT' /), 4, 'plain literal count';
is +$sh.comb(/ <["\#]> '$BUNDLE_ROOT' /), 3, 'class of quote and hash, then the literal';
is 'a"$B c'.comb(/ <-["]> '$B' /).elems, 0, 'negated class';
is 'a"$B c'.comb(/ <+["]+[a]> '$B' /).elems, 1, 'combined classes';

my $y = "Y";
is 'a"$B c "Y"'.comb(/ <["]> '$B' | '"' "$y" /).raku, '("\"\$B", "\"Y").Seq',
    'a real double-quoted atom after the class still interpolates';
is 'ab"c]d'.comb(/ <[\]"]> /).raku, '("\"", "]").Seq', 'escaped ] inside the class';
