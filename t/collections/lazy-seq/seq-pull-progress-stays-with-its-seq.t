use Test;

# The progress of a pull that died belongs to the Seq that was pulled. A
# consuming read of an inner Seq inside a map callback must not leak its
# progress into the outer Seq, whose pull the exception then aborts (#12048).

plan 6;

sub mk { (1, 2, 3).map({ die "inner" if $_ == 2; $_ }) }

for <Array eager sink> -> $m {
    my $outer = (10, 20, 30).map({ my $i = mk(); $i."$m"(); $_ + 1 });
    my @msgs;
    for ^3 { try $outer.elems; @msgs.push: $!.message }
    is @msgs.join(','), 'inner,inner,inner', "$m: each failing element is skipped in turn";
    is $outer.elems, 0, "$m: then the outer Seq is exhausted";
}
