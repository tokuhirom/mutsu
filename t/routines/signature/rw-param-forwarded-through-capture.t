use Test;

# An `is rw` parameter forwarded through a `|c` capture to another routine's
# `is rw` parameter keeps its container: the callee's write reaches the
# variable the outermost caller passed (#13921, Intl::CLDR's loader). Checked
# against rakudo.

plan 8;

sub add($x is rw) { $x += 2 }
sub fwd(|c) { add(|c) }

sub top($o is rw) { fwd($o) }
my $a = 0; top($a);
is $a, 2, 'rw parameter -> |c -> rw parameter';

sub top-twice($o is rw) { fwd($o); fwd($o); $o }
my $b = 0;
is top-twice($b), 4, 'the forwarding routine sees both writes';
is $b, 4, '...and so does its caller';

sub top-nested($o is rw) { top($o) }
my $c = 0; top-nested($c);
is $c, 2, 'two rw levels before the capture';

class W {
    method mk(|c) { self.add(|c) }
    method add($blob, $offset is rw) { $offset += 1; self }
}
sub via-method($offset is rw) { W.mk('x', $offset); W.mk('x', $offset); $offset }
my $d = 0;
is via-method($d), 2, 'a method forwarding its capture to a method';
is $d, 2, '...reaches the caller';

class Item {
    has $.v;
    method new(|c) { self.bless!take: |c }
    method !take(\blob, uint64 $offset is rw) { $!v = blob.substr($offset, 1); $offset += 1; self }
}
class Pair2 {
    has @.items;
    method new(\blob, uint64 $offset is rw) {
        my @items = Item.new(blob, $offset), Item.new(blob, $offset);
        self.bless(:@items)
    }
}
class Outer {
    has $.inner;
    method new(\blob, uint64 $offset is rw) { self.bless(inner => Pair2.new(blob, $offset)) }
}
my uint64 $off = 0;
my $o = Outer.new('abcd', $off);
is $o.inner.items.map(*.v).join(','), 'a,b', 'a loader threading a native offset through new(|c)';
is $off, 2, '...advances the offset for the outermost caller';
