use Test;

# Hyper function-ops (`%a >>[&op]<< %b`) over SetHash/BagHash/MixHash operands
# apply the op to each value key-by-key and rebuild the QuantHash type: Set
# keeps truthy keys, Bag keeps strictly-positive weights, Mix keeps non-zero
# weights. Supporting machinery: a multi-param `for` `%`-binding keeps the
# QuantHash type, a Seq initializer coerces (`my %x is SetHash = ...map(...)`),
# and reassigning a Seq of pairs to a bound QuantHash keeps its type.
# Mirrors roast/S03-metaops/infix.t.

plan 16;

my &op = &[+];
my &sub = &[-];
my &addinto = &[+=];

# --- BagHash: weights add, type preserved ---
{
    my %ba is BagHash = 'a'..'c' Z=> 1..3;
    my %bb is BagHash = 'a'..'c' Z=> 6..8;
    is-deeply (%ba >>[&op]<< %bb), ('a'=>7, 'b'=>9, 'c'=>11).BagHash, 'BagHash >>[&+]<<';
    is-deeply (%ba <<[&op]>> 3),   ('a'=>4, 'b'=>5, 'c'=>6).BagHash,  'BagHash <<[&+]>> scalar';
    # subtraction drops non-positive weights -> empty BagHash
    is-deeply (%ba >>[&sub]<< %bb), ().BagHash, 'BagHash subtraction drops non-positive';
}

# --- MixHash: non-zero weights kept (including negatives) ---
{
    my %ma is MixHash = 'a'..'c' Z=> 1..3;
    my %mb is MixHash = 'a'..'c' Z=> 6..8;
    is-deeply (%ma >>[&op]<< %mb),  ('a'=>7, 'b'=>9, 'c'=>11).MixHash,   'MixHash >>[&+]<<';
    is-deeply (%ma >>[&sub]<< %mb), ('a'=>-5, 'b'=>-5, 'c'=>-5).MixHash, 'MixHash keeps negatives';
}

# --- SetHash: truthy keys kept; result is a SetHash ---
{
    my %sa is SetHash = 'a'..'c' Z=> 1..3;
    my %sb is SetHash = 'a'..'c' Z=> 6..8;
    my $r = %sa >>[&op]<< %sb;
    isa-ok $r, SetHash, 'SetHash hyper result is a SetHash';
    is $r.elems, 3, 'SetHash hyper keeps all members';
    ok $r{'a'} && $r{'b'} && $r{'c'}, 'SetHash hyper members present';
}

# --- multi-param `for` binding preserves the QuantHash type ---
{
    my %ba is BagHash = (a => 1, b => 2);
    my $seen;
    for (%ba, %ba) -> %x, %y { $seen = %x.WHAT; last }
    is $seen.gist, '(BagHash)', 'multi-param for-binding keeps BagHash';
}

# --- Seq initializer coerces to the declared QuantHash ---
{
    my %sa is SetHash = 'a'..'c' Z=> 1..3;
    my %r is SetHash = %sa.map: { .key => .value + 1 };
    is %r.elems, 3, 'SetHash initialized from a Seq of pairs';
    my %ba is BagHash = 'a'..'c' Z=> 1..3;
    my %rb is BagHash = %ba.map: { .key => .value + 1 };
    is-deeply %rb, ('a'=>2, 'b'=>3, 'c'=>4).BagHash, 'BagHash initialized from a Seq of pairs';
}

# --- reassigning a Seq of pairs to a bound QuantHash keeps its type ---
{
    my %ba is BagHash = (a => 1, b => 2);
    my %reset = %ba.pairs;
    for (%ba,) -> %a {
        %a = %reset.pairs;
        is %a.WHAT.gist, '(BagHash)', 'reassigning pairs keeps BagHash';
        is %a.elems, 2, 'reassigned BagHash has the right size';
    }
}

# --- non-dwim scalar against a QuantHash dies ---
{
    my %ba is BagHash = (a => 1, b => 2);
    dies-ok { %ba >>[&op]<< 3 }, 'non-dwim scalar against a BagHash dies';
}

# --- writeback meta-op mutates the bound QuantHash ---
{
    my %ba is BagHash = (a => 1, b => 2);
    my %bb is BagHash = (a => 10, b => 20);
    for (%ba, %bb) -> %a, %b {
        %a >>[&addinto]<< %b;
        is-deeply %a, (a => 11, b => 22).BagHash, '>>[&+=]<< writes back to the BagHash';
        last;
    }
}

# --- plain Hash hyper is unaffected ---
{
    my %h = (a => 1, b => 2);
    my %g = (a => 10, b => 20);
    is-deeply (%h >>[&op]<< %g), {a => 11, b => 22}, 'plain Hash hyper still yields a Hash';
}
