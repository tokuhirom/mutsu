use Test;

# `.map` batches the source list by the block's positional-parameter count
# (its `.count`). Previously a block that required full signature binding
# (sigilless `\a`, typed, optional or defaulted parameters) was mis-called
# with a single element per iteration, raising "Too few positionals passed"
# for a legitimate multi-parameter block over a List/Seq source.

plan 11;

# sigilless multi-parameter block over a List
is-deeply (1, 2, 3, 4).map(-> \a, \b { "{a}-{b}" }), ("1-2", "3-4").Seq,
  'sigilless 2-param block chunks a List by 2';

# sigilless multi-parameter block over a Seq/Range
is-deeply (1 .. 4).map(-> \a, \b { "{a}-{b}" }), ("1-2", "3-4").Seq,
  'sigilless 2-param block chunks a Range by 2';

# three sigilless parameters
is-deeply (1, 2, 3, 4, 5, 6).map(-> \a, \b, \c { a + b + c }), (6, 15).Seq,
  'sigilless 3-param block chunks by 3';

# single sigilless parameter still works (arity 1)
is-deeply (1, 2, 3).map(-> \a { a * 2 }), (2, 4, 6).Seq,
  'sigilless 1-param block maps element-wise';

# optional trailing parameter: count is finite, so batch is 2; a short final
# chunk binds the missing slot to its default (Any here)
is-deeply (1, 2, 3).map(-> $a, $b? { "$a-{$b // 'x'}" }), ("1-2", "3-x").Seq,
  'optional 2nd param batches by 2 and tolerates a short final chunk';

# defaulted trailing parameter
is-deeply (1, 2, 3, 4).map(-> $a, $b = 99 { "$a-$b" }), ("1-2", "3-4").Seq,
  'defaulted 2nd param batches by 2';

# a required-arity short final chunk dies (matches raku)
dies-ok { (1, 2, 3).map(-> \a, \b { a }).eager },
  'short final chunk of a required 2-param block dies';

# slurpy-only block: count is infinite, arity 0 -> batch 1
is-deeply (1, 2, 3, 4).map(-> *@a { @a.elems }), (1, 1, 1, 1).Seq,
  'slurpy-only block maps element-wise';

# one required + slurpy: arity 1 -> batch 1
is-deeply (1, 2, 3, 4).map(-> $a, *@b { "$a:{@b.elems}" }),
  ("1:0", "2:0", "3:0", "4:0").Seq,
  'required-plus-slurpy block batches by the required arity';

# sigil multi-parameter block (unchanged fast path) still batches
is-deeply (1, 2, 3, 4).map(-> $a, $b { "$a-$b" }), ("1-2", "3-4").Seq,
  'sigil 2-param block chunks by 2';

# sigilless params flow into a Pair.new call
is-deeply (1, 2, 3, 4).map(-> \a, \b { Pair.new(a, b) }), (1 => 2, 3 => 4).Seq,
  'sigilless params usable as Pair.new arguments';
