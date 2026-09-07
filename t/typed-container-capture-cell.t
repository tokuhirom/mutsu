use v6;
use Test;

# ADR-0055's capture cell dichotomy: an escaping closure's capture of an
# `@`/`%` lexical it also MUTATES is not vouched, so the binding is
# distinguished from a same-named container in whatever frame happens to be
# calling by boxing the declaration into a shared cell
# (`box_decl_local_container_cell`). A container carrying an ELEMENT type
# constraint (`my Int @a`) was refused that cell, so it stayed hijackable:
#
#     my Int @a = 1, 2; @a.push(3);
#     my $f = -> { @a.elems };
#     sub collide() { my Int @a = 9; $f.() }
#     collide();          # raku: 3   mutsu was: 1
#
# The refusal was there for the CONTAINER type traits (`my %h is BagHash`),
# whose declaration store has to keep flowing through the assignment chokepoint
# that coerces the QuantHash. But those never reached this refusal at all --
# `is BagHash` is invisible to `var_type_constraint`, which is why
# `CompiledCode::compute_free_vars` carries a separate `ApplyVarTrait` name
# scan for them. So the refusal only ever caught the element-constraint case,
# which ADR-0042 made a property of the container and which therefore survives
# the cell intact.
#
# Every expectation below was measured against rakudo 2026.07.

plan 14;

# --- the ticket's repro, and its `%` and `Str` twins --------------------
my Int @a = 1, 2;
@a.push(3);
my $f1 = -> { @a.elems };
sub c1() { my Int @a = 9; $f1.() }
is c1(), 3, 'an element-typed array capture keeps its own binding';

my Str @b = 'x', 'y';
@b.push('z');
my $f2 = -> { @b.elems };
sub c2() { my Str @b = 'q'; $f2.() }
is c2(), 3, 'and so does a Str-typed one';

my Str %h = a => 'x', b => 'y';
my $f3 = -> { %h.elems };
sub c3() { my Str %h = c => 'z'; $f3.() }
is c3(), 2, 'an element-typed hash too';

# --- the cell must not cost the element check --------------------------
# ADR-0042 made the constraint a property of the container, so a write that
# reaches the array THROUGH its cell still re-checks it.
my Int @t = 1, 2;
my $f4 = -> { @t.elems };
sub c4() { my Int @t = 9; $f4.() }
c4();
is @t.WHAT.^name, 'Array[Int]', 'the celled array keeps its parameterised type';
dies-ok { @t.push("not an Int") }, 'and still rejects a bad element';
is @t.elems, 2, 'which leaves it unchanged';

# A write through the closure is type-checked the same way.
my Int @u = 1, 2;
my $f5 = -> { @u.push("nope"); @u.elems };
dies-ok { $f5.() }, 'a push through the capture is type-checked too';

# --- and it must not cost the container traits -------------------------
# These take the separate `ApplyVarTrait` route, so they must keep coercing.
my %bag is BagHash = a => 1, b => 0, c => 2;
is %bag.elems, 2, 'my %h is BagHash still drops its zero-weight key';
is %bag.WHAT.^name, 'BagHash', 'and is still a BagHash';

my %set is SetHash = <a b a>;
is %set.elems, 2, 'my %h is SetHash still de-duplicates';

# --- the untyped baselines, which already worked -----------------------
my @p = 1, 2;
@p.push(3);
my $f6 = -> { @p.elems };
sub c6() { my @p = 9; $f6.() }
is c6(), 3, 'the untyped array baseline is unchanged';

# A read-only capture is VOUCHED rather than celled; the typed form must keep
# taking that route and not regress into a cell it does not need.
my Int @v = 1, 2, 3;
my $f7 = -> { @v.elems };
sub c7() { my Int @v = 9; $f7.() }
is c7(), 3, 'a vouched typed capture still resolves to its own binding';

# --- the NATIVE element types keep flowing through the chokepoint -------
# Their elements are raw machine slots, not Values, so a `ContainerRef` in
# front of the container breaks native/atomic element access: celling a
# `my atomicint @values` made `cas(@values[0], ...)` fail with "Cannot convert
# value to native integer type 'int'" (`roast/S17-lowlevel/cas-int.t`). They are
# refused the cell for that reason, and so stay hijackable -- the residue this
# pins deliberately, rather than a behaviour to rely on.
my atomicint @at;
@at[0] = 0;
cas(@at[0], 0, 7);
is @at[0], 7, 'cas on an atomicint array element still works';

my int @ni = 1, 2, 3;
my $f8 = -> { @ni.elems };
sub c8() { my int @ni = 9; $f8.() }
ok c8() ~~ Int, 'a native-typed array capture still returns an Int (no cell)';
