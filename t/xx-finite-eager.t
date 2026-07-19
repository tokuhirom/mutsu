use Test;

# A finite `xx` count is eager in Raku: the result materializes all N
# elements and is NOT lazy. mutsu previously capped large finite repeats at a
# 4096-element lazy cache (`.is-lazy` True, `.elems` wrong / truncated).
# See raku-doc/doc/Language/traps.rakudoc (append trap) and the QA doc-diff campaign.

plan 9;

# Direct .elems on a large finite repeat (was: "Cannot .elems a lazy list").
is (1 xx 999999).elems, 999999, 'finite xx: direct .elems is the full count';

# Not lazy for a finite count.
is (1 xx 999999).is-lazy, False, 'finite xx is not lazy';

# Assigning to an array materializes all N.
my @a = 1 xx 999999;
is @a.elems, 999999, 'assigning finite xx to @a keeps all elements';

# append of a large finite repeat (the traps.rakudoc example).
my @b;
@b.append: @a;
is @b.elems, 999999, 'append of a 999999-element list keeps all';

# Just past the old EAGER_LIMIT (10_000) boundary stays correct.
is (1 xx 10001).elems, 10001, 'just past the old eager limit is eager';
is (1 xx 10001).is-lazy, False, 'just past the old eager limit is not lazy';

# Infinite repeat is still lazy (unchanged).
is (1 xx *).is-lazy, True, 'infinite xx * stays lazy';
is (1 xx *).head(3).raku, '(1, 1, 1).Seq', 'infinite xx * still yields elements';

# do-block LHS re-evaluates once per finite element.
my $i = 0;
my @c = do { $i++ } xx 5;
is @c.join(','), '0,1,2,3,4', 'do-block xx N re-evaluates eagerly N times';
