use Test;

# A Raku method has an implicit `*%_`: a named argument it does not declare is
# swallowed. The built-in method rows state which names they bind, and a name
# no row of the method binds is dropped (ADR-11276 slice 5); a name another row
# of the method binds (an interpreter twin, a slurpy `*%unit`) still reaches it.

plan 17;

# Undeclared names are invisible.
is "abc".uc(:zzz), 'ABC', 'uc swallows an undeclared named';
is-deeply "a b".words(:zzz).List, ("a", "b"), 'words swallows it';
is-deeply <a b a>.unique(:zzz).List, ("a", "b"), 'unique swallows it';
is (1, 2, 3).AT-POS(1, :zzz), 2, 'AT-POS does not count it as an index';
is-deeply 10.polymod(3, :zzz).List, (1, 3), 'polymod does not read it as a modulus';
is "abc".subst("b", "x", :qqzz9), 'axc', 'subst swallows an unknown adverb';
is 255.base(16, :zzz), 'FF', 'base swallows it';
is 3.sqrt(:zzz), 3.sqrt, 'sqrt swallows it';
is "abc".chars(:zzz), 3, 'chars swallows it';
is-deeply [3, 1, 2].sort(:zzz).List, (1, 2, 3), 'sort swallows it';
is 1.5.Str(:zzz), '1.5', 'Rat.Str swallows it';

# Declared names still reach the method.
is-deeply <a b A>.unique(:as(&lc)).List, ("a", "b"), 'unique keeps :as';
is "aaa".subst("a", "x", :g), 'xxx', 'subst keeps :g';
is-deeply (1..10).rotor(3, :partial).List, ((1, 2, 3), (4, 5, 6), (7, 8, 9), (10,)),
    'rotor keeps :partial';
is 12.Str(:superscript), '¹²', 'Int.Str keeps :superscript';

# `later`/`earlier` take `*%unit`: every named is a unit, an unknown one fails.
is Date.new(2024, 1, 31).later(:1month), '2024-02-29', 'later reads its unit';
dies-ok { Date.new(2024, 1, 31).later(:3fortnights) }, 'an unknown unit is not swallowed';
