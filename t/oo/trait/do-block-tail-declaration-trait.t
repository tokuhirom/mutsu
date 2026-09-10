use v6;
use Test;

# A declaration that is a `do` block's TAIL statement must still get its
# variable trait. The block-final `Stmt::VarDecl` arm of `compile_block_inline`
# is a hand-inlined compile path -- needed so a block-final declaration yields
# its value -- and it emitted no `ApplyVarTrait` at all, so
# `my $z = do { my %u is SetHash }` produced a plain `Hash` (GH #7583).
#
# It is not specific to container traits: `is default(...)` was lost the same
# way, for both sigils, which is what shows the trait was never applied rather
# than applied too late.

# NOTE: every declaration below uses its OWN variable name on purpose. A
# same-named `my %h` elsewhere in the same file cancels an `is default(...)`
# container (GH #7621) -- a separate, pre-existing bug that has nothing to do
# with the tail position, and one this file must not accidentally exercise.

plan 16;

# --- container traits as the tail --------------------------------------
{
    my $z = do { my %sh is SetHash };
    is $z.^name, 'SetHash', 'do-block tail: is SetHash';
}
{
    my $z = do { my %bh is BagHash };
    is $z.^name, 'BagHash', 'do-block tail: is BagHash';
}
{
    my $z = do { my %mh is MixHash };
    is $z.^name, 'MixHash', 'do-block tail: is MixHash';
}
{
    my $z = do { my @buf is Buf };
    is $z.^name, 'Buf', 'do-block tail: is Buf';
}

# --- `is default(...)`, whose argument must be compiled before the op ---
{
    my $z = do { my %hd is default(42) };
    is $z<nope>, 42, 'do-block tail: a hash keeps its is default(...)';
}
{
    my $y = do { my $sd is default(7) };
    is $y, 7, 'do-block tail: a scalar keeps its is default(...)';
}
{
    my $z = do { my @ad is default('x') };
    is $z[5], 'x', 'do-block tail: an array keeps its is default(...)';
}

# --- the same declaration as a bare block used as a value --------------
{
    sub f { my %fsh is SetHash }
    is f().^name, 'SetHash', "a routine's tail declaration keeps its trait";
}

# --- controls that already worked and must not move --------------------
{
    is (do { my %stmt is SetHash; %stmt.^name }), 'SetHash',
       'an ordinary statement inside the block was already correct';
}
{
    my $z = (my %pe is SetHash);
    is $z.^name, 'SetHash', 'the parenthesised expression position is unmoved';
}
{
    my $z = do { my $plain = 5 };
    is $z, 5, 'an untraited tail declaration still yields its value';
}
{
    my $z = do { my Int $typed = 5 };
    is $z, 5, 'a typed untraited tail declaration still yields its value';
    is $z.^name, 'Int', 'and keeps its type';
}
{
    my $z = do { my $bare };
    nok $z.defined, 'a bare tail declaration still yields an undefined value';
}

# --- the declared variable itself is still the traited container -------
{
    my %earlier;
    my $z = do { %earlier = SetHash.new; my %v is SetHash };
    is $z.^name, 'SetHash', 'the trait applies to the tail declaration, not an earlier one';
}
{
    my $z = do { my %rt is SetHash };
    $z<a> = True;
    is $z<a>, True, 'the returned container behaves as its traited type';
}
