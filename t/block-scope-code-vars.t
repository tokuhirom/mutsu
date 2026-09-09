use Test;

# A carrier block (`do { }`, a `where` clause, a regex `<?{ }>` assertion) saves
# and restores the `&`-routine bindings visible in the scope it runs in, so a
# block-local `sub` cannot leak into the caller and an enclosing binding is
# still there afterwards. #7575 replaced the two whole-env scans that did that
# with an index of exactly those keys; these are the semantics the index has to
# keep.

plan 10;

sub outer() { "outer" }

# A `sub` declared inside a carrier block does not leak out of it.
my $inner-value = do { sub inner-only() { "inner" }; inner-only() };
is $inner-value, "inner", "a block-local sub is callable inside the block";
ok !::('&inner-only').defined, "a block-local sub does not leak into the caller";
is outer(), "outer", "an existing outer sub still resolves after the block";

# Repeated carrier blocks each get a clean slate.
for ^3 -> $i {
    my $v = do { sub loop-local($n) { "n$n" }; loop-local($i) };
    is $v, "n$i", "iteration $i sees its own block-local sub";
}

# A regex assertion body is a carrier block run once per cursor position; the
# routines it calls must stay resolvable for every one of them.
sub keep($c) { $c eq "b" }
is "abc".subst(/ <?{ keep("b") }> b /, "X"), "aXc",
    "a sub called from a regex assertion resolves at every cursor position";
is keep("b"), True, "the sub is still bound after the match";

# `where` clauses are carrier blocks too.
sub small($n where { $^x < 10 }) { $n * 2 }
is small(4), 8, "a where clause runs as a carrier block";
is outer(), "outer", "outer bindings survive a where clause";
