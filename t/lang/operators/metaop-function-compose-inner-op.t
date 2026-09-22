use Test;

# #9050: `o` (and its Unicode alias `∘`) works as a plain infix, but not as
# the inner operator of a `Z`/`X` meta-operator or a reduction -- the meta
# path's per-pair leaf dispatch (`eval_infix_leaf`) had no arm for
# composition and fell through to `apply_reduction_op`'s catch-all
# "Unsupported reduction operator: o". Plain `infix:<o>` never reaches that
# function at all (it compiles straight to `OpCode::FunctionCompose`, which
# calls `Interpreter::compose_callables` directly), so the two paths had
# diverged.
#
# `Z∘`/`X∘` fail identically to `Zo`/`Xo` -- this is not a Unicode-alias
# problem, the alias is folded to `o` before the meta path sees it -- but
# both spellings are pinned below anyway.

plan 9;

my &f = * + 1;
my &g = * * 2;

# --- Zip over composition. ---
my @zipped = (&f, &g) Zo (&g, &f);
is @zipped.elems, 2, 'Zo zips two Callables into two composed Callables';
is @zipped[0](3), 7, 'the first composed Callable is f o g: f(g(3)) == f(6) == 7';
is @zipped[1](3), 8, 'the second composed Callable is g o f: g(f(3)) == g(4) == 8';

# --- The Unicode alias agrees. ---
my @zipped-uni = (&f, &g) Z∘ (&g, &f);
is @zipped-uni.elems, 2, 'Z∘ (the Unicode alias) zips the same way as Zo';
is @zipped-uni[0](3), 7, 'the Unicode alias first composed Callable agrees with Zo';

# --- Cross over composition. ---
my @crossed = (&f,) Xo (&g,);
is @crossed.elems, 1, 'Xo crosses two single-element lists into one composed Callable';
is @crossed[0](3), 7, 'the crossed composed Callable is f o g: f(g(3)) == 7';

# --- Reduction over composition. ---
my &h = * - 1;
my &composed = [o] (&f, &g, &h);
is composed(3), 5, '[o] reduces three Callables right-to-left: f(g(h(3))) == f(g(2)) == f(4) == 5';

# --- The plain infix (already correct) must keep working. ---
my &plain = &f o &g;
is plain(3), 7, 'the plain infix:<o> is unaffected';
