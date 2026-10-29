use Test;

# `%!h = <plain Hash>` into an untyped attribute skips the coercion probes.
# Every shape that is not a plain Hash into a plain attribute must still be
# coerced, typed, copied or refused as before.

plan 16;

class P {
    has %!h;
    has Int %!typed;
    has %!obj{Any};
    method set(%x) { %!h = %x; self }
    method set-any($x) { %!h = $x; self }
    method set-typed(%x) { %!typed = %x; self }
    method set-obj(%x) { %!obj = %x; self }
    method h() { %!h }
    method typed() { %!typed }
    method obj() { %!obj }
}

my %src = a => 1, b => 2;
my $p = P.new.set(%src);
is-deeply $p.h, {a => 1, b => 2}, 'a plain Hash is stored';
%src<a> = 99;
is $p.h<a>, 1, 'the attribute owns a copy';
$p.h<c> = 3;
is %src<c>:exists, False, 'writes to the attribute do not reach the source';

# repeated stores through one site, including an empty hash
$p.set({}) for ^2;
is-deeply $p.h, {}, 'an empty hash replaces the contents';
$p.set(%(x => 1));
is-deeply $p.h, {x => 1}, 'and the next store follows';

# a Map or a Bag or a list is coerced as before
is-deeply P.new.set-any(Map.new((k => 1))).h, {k => 1}, 'a Map assigns its entries';
is P.new.set-any(<a 1 b 2>.Hash).h<b>, 2, 'a Hash made from a list';
is-deeply P.new.set-any((a => 1, b => 2)).h, {a => 1, b => 2}, 'a list of pairs is coerced';

# a bound value is a stored value, not an alias of the container
my $v = 5;
my %with-cell = k => $v;
$v = 6;
is P.new.set(%with-cell).h<k>, 5, 'a cell-valued entry is copied by value';

# a typed attribute still checks its values
is P.new.set-typed({ n => 1 }).typed<n>, 1, 'a typed hash attribute stores';
dies-ok { P.new.set-typed({ n => 'x' }) }, 'and still refuses a wrong value';
is P.new.set-typed({ n => 1 }).typed.of.^name, 'Int', 'and keeps its element type';

# an object hash attribute keeps its key type
my $o = P.new.set-obj(%(a => 1));
is $o.obj<a>, 1, 'an object hash attribute stores';


# two instances do not share their storage
my $q = P.new.set({ only => 'q' });
my $r = P.new.set({ only => 'r' });
is $q.h<only>, 'q', 'the first instance keeps its own value';
is $r.h<only>, 'r', 'the second has its own';
is-deeply P.new.h, {}, 'an untouched instance is empty';
