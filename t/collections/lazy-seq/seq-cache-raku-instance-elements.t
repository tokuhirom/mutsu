use Test;

# `Seq.cache` is a List to Raku, so `.raku` renders it as one even when its
# elements are class instances (which route `.raku` through leaf dispatch).
# Found with the `annotations` distribution (Buffer.STAMP gather).
plan 6;

class B { has $.x = 0 }

my $a = (B.new,).Seq.cache;
is $a.raku, '$(B.new(x => 0),)', 'cached Seq of one instance renders as a List';
is (B.new, B.new).Seq.cache.raku, '(B.new(x => 0), B.new(x => 0))',
    'cached Seq of two instances, uncontainerized';

sub mk(+@v is raw) {
    gather for @v -> Mu $x is raw {
        my $b = B.new(x => $x);
        ENTER { 1 }
        CATCH { next }
        KEEP { take-rw $b }
        my $o = 5;
    }
}
my $s = mk(8, 9);
my $c = $s.cache;
is $c.^name, 'List', 'cache of the gather is a List';
is $c.raku, '$(B.new(x => 8), B.new(x => 9))', 'gather of instances renders as a List';
is mk(8, 9).cache.raku, '(B.new(x => 8), B.new(x => 9))', 'direct cache.raku';
is (1, 2).Seq.cache.raku, '(1, 2)', 'plain values unchanged';
