use Test;

# `SetHash.set`/`.unset` and the QuantHash `.grab`/`.grabpairs` are rows of the
# one method table (ADR-11276 §9.23): `Handler::Mut` rows reached through
# `invoke_mut`, owned by the type Rakudo declares them on.

plan 42;

# --- SetHash.set / unset -------------------------------------------------
my $s = SetHash.new(1, 2, 3);
is $s.set(4).raku, 'Nil', 'set answers Nil';
is $s.sort.gist, '(1 => True 2 => True 3 => True 4 => True)', 'set adds a key';
$s.unset(1);
$s.unset(2);
is $s.sort.gist, '(3 => True 4 => True)', 'unset removes a key';
$s.set((7, 8));
is $s.sort.gist, '(3 => True 4 => True 7 => True 8 => True)',
    'a list argument names its elements as keys';
is $s.unset(7).raku, 'Nil', 'unset answers Nil';

# --- grab / grabpairs ------------------------------------------------------
my $g = SetHash.new(1);
is $g.grab, 1, 'SetHash.grab answers the grabbed key';
is $g.elems, 0, 'and removes it';
is $g.grab.raku, 'Nil', 'grab of an empty SetHash is Nil';

my $b = BagHash.new(1, 1, 2);
is $b.grab(*).elems, 3, 'BagHash.grab(*) grabs every unit of weight';
is $b.elems, 0, 'and drains the bag';

my $pairs = BagHash.new(<a a b>);
is $pairs.grabpairs(*).elems, 2, 'BagHash.grabpairs(*) grabs every key';
is $pairs.elems, 0, 'and drains the bag';

my $mm = MixHash.new(1, 2, 3);
is $mm.grabpairs(2).elems, 2, 'MixHash.grabpairs(2)';
is $mm.elems, 1, 'removes the grabbed keys';

# a Callable count is invoked with the weight (grab) or the key count (grabpairs)
my $c = BagHash.new(1, 1, 1, 2);
is $c.grab(* div 2).elems, 2, 'a Callable count to BagHash.grab sees the total weight';
is $c.total, 2, 'and removed that many units';
my $sh = SetHash.new(1, 2, 3, 4);
is $sh.grab(* div 2).elems, 2, 'a Callable count to SetHash.grab sees the key count';
is $sh.elems, 2, 'and removed that many keys';

# a named argument is not part of the signature and is ignored
my $n = SetHash.new(1, 2);
ok $n.grab(:zzz).defined, 'an undeclared named argument is ignored';
is $n.elems, 1, 'and the grab happened';

# --- the immutable owners declare them and refuse ---------------------------
for Set.new(1), Bag.new(1), Mix.new(1) -> $x {
    for <grab grabpairs> -> $name {
        throws-like { $x."$name"() }, X::Immutable, method => $name, typename => $x.^name,
            "{$x.^name}.$name is immutable";
    }
}

# --- only SetHash has set / unset -------------------------------------------
for BagHash.new(1), MixHash.new(1) -> $x {
    throws-like { $x.set(1) }, X::Method::NotFound, method => 'set', "{$x.^name} has no set";
}

# --- MixHash.grab is declared and refused (grabpairs is not) ------------------
my $mix = MixHash.new(1, 2);
dies-ok { $mix.grab(1).elems }, 'MixHash.grab is not supported';
is $mix.elems, 2, 'and removed nothing';

# --- the receiver shapes ------------------------------------------------------
class Holder { has SetHash $.q = SetHash.new(1, 2); }
my $o = Holder.new;
$o.q.unset(1);
is $o.q.elems, 1, 'a call on an attribute accessor';

my @sets;
@sets.push(SetHash.new('a', 'b'));
@sets[0].unset('a');
is @sets[0].sort.gist, '(b => True)', 'a call on an element';

my %h is SetHash = <x y>;
%h.set('z');
%h.unset('x');
is %h.sort.gist, '(y => True z => True)', 'a variable declared `is SetHash`';

class MySet is SetHash { }
my $m = MySet.new(1, 2, 3);
$m.unset(1);
is $m.sort.gist, '(2 => True 3 => True)', 'unset on a user subclass of SetHash';

# --- SetHash.set / unset take exactly one positional (#12244) -----------
{
    # SetHash.set / .unset take exactly one positional (a list is iterated one level).
    my $s = SetHash.new(1, 2, 3);
    try { $s.unset(1, 2); CATCH { default { is .^name, 'X::AdHoc', 'unset with two args throws'; like .message, /'Too many positionals passed; expected 2 arguments but got 3'/, 'unset too-many message' } } }
    is $s.elems, 3, 'a rejected unset removes nothing';

    try { $s.set(); CATCH { default { is .^name, 'X::AdHoc', 'set() throws'; like .message, /'Too few positionals passed; expected 2 arguments but got 1'/, 'set too-few message' } } }

    $s.unset((1, 2));
    is $s.elems, 1, 'unset with one list removes each element';
    $s.set((4, 5));
    is $s.elems, 3, 'set with one list adds each element';
    $s.set(9);
    ok $s{9}, 'set with a single key';
}

done-testing;
