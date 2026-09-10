use Test;

# Declared QuantHash (`my %s is SetHash/BagHash/MixHash`) initialization and
# container identity (§3):
# - a SCALAR initializer (`= <a>` — a single angle-quoted word is a Str, not a
#   List) must initialize the container, not be dropped as "no initializer"
# - subscript assignment writes through the variable's own container node, so
#   `:=` binds and closure captures observe it, while `=` copies stay detached
# - whole reassignment (`%s = <x>`) preserves the container's identity

plan 16;

# --- scalar (Str) initializer ---
{ my %s is SetHash = <a>; is %s.keys.join(','), 'a', 'SetHash Str initializer lands'; }
{ my %b is BagHash = <p>; is %b.keys.join(','), 'p', 'BagHash Str initializer lands'; }
{ my %m is MixHash = <m>; is %m.keys.join(','), 'm', 'MixHash Str initializer lands'; }
{ my %s is SetHash = <a>; %s<b> = True; is %s.keys.sort.join(','), 'a,b', 'subscript assign after Str init keeps prior contents'; }

# --- subscript assign visible through := bind ---
{ my %u is SetHash = <a b>; my %v := %u; %u<c> = True; is %v.keys.sort.join(','), 'a,b,c', 'SetHash := bind observes subscript assign'; }
{ my %u is BagHash = <k>;   my %v := %u; %u<l> = 2;    is %v.keys.sort.join(','), 'k,l', 'BagHash := bind observes subscript assign'; }
{ my %u is MixHash = <k>;   my %v := %u; %u<l> = 0.5;  is %v.keys.sort.join(','), 'k,l', 'MixHash := bind observes subscript assign'; }

# --- subscript assign visible through a closure capture ---
{ my %w is SetHash = <a>; my $cl = { %w.keys.sort.join(',') }; %w<b> = True; is $cl(), 'a,b', 'closure capture observes SetHash subscript assign'; }
{ my %x is BagHash = <p>; my $cl = { %x.keys.sort.join(',') }; %x<q> = 2;    is $cl(), 'p,q', 'closure capture observes BagHash subscript assign'; }

# --- copies stay detached ---
{ my %s is SetHash = <a>; my %t is SetHash = %s; %t<z> = True; is %s.keys.join(','), 'a', 'SetHash copy mutation does not reach source'; }
{ my %p is BagHash = <k k>; my %q is BagHash = %p; %q<m> = 3;  is %p.keys.join(','), 'k', 'BagHash copy mutation does not reach source'; }
{ my %s is SetHash = <a b>; my %t = %s; %s<c> = True; is %t.keys.sort.join(','), 'a,b', 'plain copy unaffected by source subscript assign'; }

# --- whole reassignment preserves identity (visible through := bind) ---
{ my %u is SetHash = <a b>; my %v := %u; %u = <x>; is %v.keys.join(','), 'x', 'whole reassign observed through := bind'; }

# --- reassign keeps declared type and coercion ---
{
    my %w is SetHash = <a>;
    %w = <b c>;
    %w<d> = True;
    is %w.keys.sort.join(','), 'b,c,d', 'reassign then subscript assign accumulate';
    is %w.WHAT.raku, 'SetHash', 'reassign keeps SetHash type';
}

# --- SetHash element assignment evaluates to Bool ---
{ my %s is SetHash; is-deeply (%s<k> = 2), True, 'SetHash element assign yields Bool existence'; }
