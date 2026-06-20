use Test;

# L2: `my @a = 1..*` keeps `@a` a reify-on-demand lazy array instead of capping
# it at 100k elements. `@a[N]` reifies past the old cap; reads stay lazy; the
# mutators raku rejects on a lazy list (push/pop/append) throw X::Cannot::Lazy,
# while front mutations (unshift/prepend/shift/splice/elem-assign) reify the
# prefix and operate on it.

plan 25;

# --- reads past the old 100k cap reify on demand ------------------------
{
    my @a = 1..*;
    is @a[200000], 200001, 'index past the cap reifies';
    is @a.is-lazy,  True,  'infinite-range array is lazy';
    is @a.gist,     '[...]', 'lazy array gist is [...]';
    is @a.WHAT.gist, '(Array)', 'lazy array WHAT is Array';
    is-deeply @a[^5].List, (1, 2, 3, 4, 5), 'leading slice reifies';
    is @a.head(3).List, (1, 2, 3), 'head reifies the prefix';
    is @a.first(* > 3), 4, 'first pulls until the predicate matches';
    is @a.map(* * 2).head(3).List, (2, 4, 6), 'map keeps laziness';
}

# --- L2b: map/grep stay lazy even when the cache was NOT pre-extended -----
# (A fresh `1..*` is seeded with `[1]` only; the lazy `map`/`grep` pipeline
# must pull from the sequence on demand, not read the O(1) seed cache.)
{
    my @a = 1..*;
    is @a.map(* * 2).head(5).List, (2, 4, 6, 8, 10), 'fresh map pulls past the seed';
    my @b = 1..*;
    is @b.grep(* %% 3).head(4).List, (3, 6, 9, 12), 'fresh grep pulls past the seed';
    my @c = 1..*;
    is @c.map(* * 2).grep(* > 5).head(3).List, (6, 8, 10), 'chained map/grep stays lazy';
}

# --- `.elems`/numeric coercions still throw X::Cannot::Lazy ------------
{
    my @a = 1..*;
    throws-like { @a.elems }, X::Cannot::Lazy, '.elems throws';
    throws-like { @a.Int },   X::Cannot::Lazy, '.Int throws';
    throws-like { +@a },      X::Cannot::Lazy, 'prefix + throws';
}

# --- end-mutations raku rejects on a lazy list throw -------------------
{
    throws-like { my @a = 1..*; @a.push(9) },   X::Cannot::Lazy, 'push throws';
    throws-like { my @a = 1..*; @a.pop },        X::Cannot::Lazy, 'pop throws';
    throws-like { my @a = 1..*; @a.append(9) },  X::Cannot::Lazy, 'append throws';
}

# --- front-mutations reify the prefix and operate -----------------------
{
    my @a = 1..*;
    @a.unshift(100);
    is @a[0], 100, 'unshift prepends';
    is @a[1], 1,   'unshift keeps the prefix';
}
{
    my @a = 1..*;
    is @a.shift, 1, 'shift returns the head';
    is @a[0], 2,    'shift advances the front';
}
{
    my @a = 1..*;
    @a[2] = 99;
    is-deeply @a[^5].List, (1, 2, 99, 4, 5), 'elem-assign partially reifies';
}

# --- :exists / :delete on a lazy array ---------------------------------
{
    my @a = 1..*;
    is @a[3]:exists,        True, ':exists is True within reach';
    is @a[1000000]:exists,  True, ':exists is True past the old cap';
}
{
    my @a = 1..*;
    @a[3]:delete;
    is-deeply @a[^5].List, (1, 2, 3, Any, 5), ':delete reifies and removes';
}
