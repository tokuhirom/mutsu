use Test;

# Every Raku *method* carries an implicit `*%_` slurpy named parameter, so a
# named argument the method does not declare is silently swallowed. That holds
# for built-in methods just as much as for user-defined ones. Subs have no such
# implicit parameter, so they must still reject an unexpected named argument.

plan 39;

# --- Nameds the method does not declare are ignored (several types) ----------

# `Real.log`'s $base is POSITIONAL, so `:base(2)` binds nothing and this is
# plain `4.log` -- the case that first surfaced the bug.
is 4.log(:base(2)), 4.log, 'Int.log(:base(2)) ignores the named and is 4.log';
is "abc".uc(:foo), 'ABC', 'Str.uc ignores an unknown named';
is "abc".uc(:foo, :bar), 'ABC', 'Str.uc ignores several unknown nameds';
is "abc".chars(:foo), 3, 'Str.chars ignores an unknown named';
is "abc".flip(:foo), 'cba', 'Str.flip ignores an unknown named';
is "abc".ords(:foo).join(','), '97,98,99', 'Str.ords ignores an unknown named';
is "a b".words(:foo).join('|'), 'a|b', 'Str.words ignores an unknown named';
is "12".Int(:foo), 12, 'Str.Int ignores an unknown named';
is 3.7.round(:foo), 4, 'Rat.round ignores an unknown named';
is 255.base(16, :foo), 'FF', 'Int.base keeps its positional and ignores a named';
is 3.7.round(1, :foo), 4, 'Rat.round keeps its positional next to a named';
is 6.sqrt(:foo), 6.sqrt, 'Int.sqrt ignores an unknown named';
is (1, 2, 3).elems(:foo), 3, 'List.elems ignores an unknown named';
is (1, 2, 3).join("-", :foo), '1-2-3', 'List.join keeps its positional';
is (1, 2, 3).reverse(:foo).join(','), '3,2,1', 'List.reverse ignores an unknown named';
is (1, 2, 3).head(2, :foo).join(','), '1,2', 'List.head keeps its positional';
is (1, 2, 3).min(:foo), 1, 'List.min ignores an unknown named';
is (1, 2, 3).max(:foo), 3, 'List.max ignores an unknown named';
is (1, 2, 3).kv(:foo).join(','), '0,1,1,2,2,3', 'List.kv ignores an unknown named';
is (1, 2, 2).unique(:foo).join(','), '1,2', 'List.unique ignores an unknown named';
is %(a => 1).keys(:foo).join(','), 'a', 'Hash.keys ignores an unknown named';
is %(a => 1).values(:foo).join(','), '1', 'Hash.values ignores an unknown named';
is set(1).keys(:foo).join(','), '1', 'Set.keys ignores an unknown named';
is bag(1, 1, 2).total(:foo), 3, 'Bag.total ignores an unknown named';

# A `Seq` body is single-use: swallowing the named must not consume it twice.
is (1, 2, 3).map({ $_ * 2 }).join("-", :foo), '2-4-6',
   'a Seq receiver is consumed exactly once while the named is swallowed';

# --- Adverbs the method DOES declare keep working ---------------------------

is "a,b,,c".split(",", :skip-empty).join('|'), 'a|b|c', ':skip-empty is honoured';
is "a,b,,c".split(",", :skip-empty, :nonsense).join('|'), 'a|b|c',
   ':skip-empty is honoured alongside an unknown named';
is "a,b,,c".split(",", :nonsense).join('|'), 'a|b||c',
   'an unknown named does not become split\'s $limit';
is "abc".comb(:nonsense).join('|'), 'a|b|c',
   'an unknown named does not become comb\'s matcher';
is "abcdef".comb(2, :nonsense).join('|'), 'ab|cd|ef',
   'comb keeps its positional chunk size next to an unknown named';
is "hello".substr-eq("ELL", 1, :i), True, ':i is honoured by substr-eq';
is "hello".substr-eq("ELL", 1, :i, :nonsense), True,
   ':i is honoured by substr-eq alongside an unknown named';
is "hello".contains("ELL", :i), True, ':i is honoured by contains';
is "hello".contains("ELL", :i, :nonsense), True,
   ':i is honoured by contains alongside an unknown named';

# --- User-defined methods behave the same (the control) ---------------------

class C {
    method m() { 42 }
    method n($a) { $a * 2 }
}
is C.new.m(:foo), 42, 'a user-defined method swallows an unknown named too';
is C.new.n(3, :foo), 6, 'a user-defined method keeps its positional';

# --- Subs have NO implicit *%_ and must still reject an unexpected named -----

sub takes-nothing() { 42 }
dies-ok { takes-nothing(|(:foo)) },
        'a sub still rejects an unexpected named argument';

# --- A wrong-arity POSITIONAL call must still die ---------------------------

dies-ok { "abc".uc("x") }, 'a surplus positional on a 0-ary native still dies';
dies-ok { 4.log(1, 2, 3) }, 'a surplus positional on log still dies';

done-testing;
