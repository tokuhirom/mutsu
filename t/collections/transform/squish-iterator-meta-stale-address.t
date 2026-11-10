use Test;

plan 1;

# A `squish(:as, :with)` Seq that is never `.iterator`ed must not leave metadata
# behind that a later, unrelated Seq allocated at the same address picks up.
my $list = (^1000).pick(*).List;
my $as = * div 10;
my $bad = 0;
for ^300 {
    my $dropped = $list.Seq.squish(:$as, :with(&[==])).List;
    my $it = $list.squish.iterator;
    my $buf = IterationBuffer.new;
    $it.push-all($buf);
    $bad++ if $buf.elems != 1000;
}
is $bad, 0, 'a plain squish iterator never inherits another squish\'s :as/:with';

# vim: expandtab shiftwidth=4
