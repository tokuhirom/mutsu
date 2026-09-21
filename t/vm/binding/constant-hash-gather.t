use v6;
use Test;

# A `%`-sigiled constant initialized from `gather { take ... }` must reify
# the LazyList before pairing it up, the same way `coerce_object_to_hash`
# already does for the non-constant `my %h = gather {...}` case. Without
# it, `coerce_constant_hash_value` fell to its opaque-scalar fallback and
# treated the whole (unforced) LazyList as ONE item, dying with
# X::Hash::Store::OddNumber ("found 1 element(s)") even for an even number
# of `take`n pairs.
#
# Found via CSS::Minifier 0.0.14's Util.rakumod, whose `%HEX-TO-NAME`
# constant is built via `%map.append: %NAMED.map({ ... })`, which in turn
# made `use CSS::Minifier::Normalizer` (which pulls in a `constant
# %LONGHAND-TO-SHORTHAND = gather for %SHORTHAND.kv -> $sh, @lhs { take
# $_ => $sh for @lhs; };`) die on load.

plan 4;

my constant %h = gather { take 'a' => 1; take 'b' => 2; };
is %h.WHAT.gist, '(Map)', 'constant % from a gather/take Seq is a Map';
is-deeply %h.sort, (a => 1, b => 2).sort, 'pairs survive reification';

my constant %source = margin => <margin-top margin-bottom>,
                       padding => <padding-top padding-bottom>;
my constant %shorthand =
    gather for %source.kv -> $sh, @lhs {
        take $_ => $sh for @lhs;
    };
is %shorthand.elems, 4, 'nested for/take inside gather reifies every pair';
is %shorthand<margin-top>, 'margin', 'a specific longhand->shorthand pair is correct';
