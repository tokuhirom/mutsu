use v6;
use Test;

# Deleting/assigning a key on an immutable QuantHash (Set/Bag/Mix) is a
# read-only modification: raku throws X::Assignment::RO (not X::Immutable),
# with the exception's .typename naming the container type. rakudo changed
# this from X::Immutable; roast S02-types/{bag,mix}.t and S32-basics/xxKEY.t
# were updated to match.

plan 15;

my $b = <a a b>.Bag;
my $s = <a b>.Set;
my $m = (a => 1.5, b => 2).Mix;

# --- :delete adverb ---------------------------------------------------------
throws-like { $b<a>:delete }, X::Assignment::RO, :typename<Bag>, 'Bag $b<a>:delete';
throws-like { $s<a>:delete }, X::Assignment::RO, :typename<Set>, 'Set $s<a>:delete';
throws-like { $m<a>:delete }, X::Assignment::RO, :typename<Mix>, 'Mix $m<a>:delete';

# --- DELETE-KEY -------------------------------------------------------------
throws-like { $b.DELETE-KEY('a') }, X::Assignment::RO, :typename<Bag>, 'Bag.DELETE-KEY';
throws-like { $s.DELETE-KEY('a') }, X::Assignment::RO, :typename<Set>, 'Set.DELETE-KEY';
throws-like { $m.DELETE-KEY('a') }, X::Assignment::RO, :typename<Mix>, 'Mix.DELETE-KEY';

# --- ASSIGN-KEY -------------------------------------------------------------
throws-like { $b.ASSIGN-KEY('a', 42) }, X::Assignment::RO, :typename<Bag>, 'Bag.ASSIGN-KEY';
throws-like { $s.ASSIGN-KEY('a', True) }, X::Assignment::RO, :typename<Set>, 'Set.ASSIGN-KEY';
throws-like { $m.ASSIGN-KEY('a', 3) }, X::Assignment::RO, :typename<Mix>, 'Mix.ASSIGN-KEY';

# --- message shape ----------------------------------------------------------
throws-like { $b<a>:delete }, X::Assignment::RO,
    message => /^ 'Cannot modify an immutable Bag'/, 'Bag :delete message';

# --- mutable hashes are unaffected -----------------------------------------
my $bh = <a a b>.BagHash;
lives-ok { $bh<a>:delete }, 'BagHash :delete lives';
is $bh<a>, 0, 'BagHash key removed';

my %h = a => 1, b => 2;
lives-ok { %h<a>:delete }, 'plain Hash :delete lives';
is %h.elems, 1, 'plain Hash key removed';

my $sh = <a b>.SetHash;
lives-ok { $sh.DELETE-KEY('a') }, 'SetHash.DELETE-KEY lives';
