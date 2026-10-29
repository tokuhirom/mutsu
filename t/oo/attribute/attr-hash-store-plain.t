use Test;

plan 7;

# `%!h = ...` / `@!a = ...` in a method: the attribute cell holding a plain
# Hash/Array is neither tied nor mixed, so the store goes straight to the
# ordinary assignment; a tied or mixed attribute still routes through STORE.

class Plain {
    has %.h;
    has @.a;
    method set-h(%x) { %!h = %x; self }
    method set-a(@x) { @!a = @x; self }
}
my $p = Plain.new;
for 1..3 -> $i {
    $p.set-h({ k => $i });
    $p.set-a([$i, $i + 1]);
}
is-deeply $p.h, { k => 3 }, 'plain hash attribute is replaced by the assignment';
is-deeply $p.a, [3, 4], 'plain array attribute is replaced by the assignment';

# Assignment copies: later changes to the source do not show in the attribute.
my %src = a => 1;
$p.set-h(%src);
%src<a> = 99;
is $p.h<a>, 1, 'the attribute holds a copy';

# A typed hash attribute still checks its elements.
class Typed {
    has Int %.h;
    method set(%x) { %!h = %x; self }
}
my $t = Typed.new;
$t.set({ a => 1 }) for ^3;
is $t.h<a>, 1, 'typed hash attribute accepts matching values when warm';
dies-ok { $t.set({ a => 'x' }) }, 'typed hash attribute rejects a wrong element when warm';

# A tied (user-class) container attribute keeps its identity.
class Tie {
    has %!store;
    method STORE(\v, |c) { %!store = v.Hash; self }
    method AT-KEY($k) { %!store{$k} }
    method keys { %!store.keys }
}
class Holder {
    has %.t is Tie;
    method set(%x) { %!t = %x; self }
}
my $h = Holder.new;
$h.set({ x => 1 });
$h.set({ y => 2 });
is $h.t.keys.sort.List, ('y',), 'a tied attribute goes through its own STORE';
is $h.t<y>, 2, 'and reads back through AT-KEY';
