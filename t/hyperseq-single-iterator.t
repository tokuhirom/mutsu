use Test;

plan 6;

# HyperSeq/RaceSeq allow only a single iterator (rakudo #4413):
# a second .iterator throws X::Seq::Consumed.

throws-like { (.iterator, .iterator) given (^10).hyper },
    X::Seq::Consumed,
    'second .iterator on a HyperSeq throws X::Seq::Consumed';

throws-like { (.iterator, .iterator) given (^10).race },
    X::Seq::Consumed,
    'second .iterator on a RaceSeq throws X::Seq::Consumed';

# A fresh HyperSeq/RaceSeq each has its own iterator budget.
lives-ok { (^10).hyper.iterator; (^10).hyper.iterator },
    'distinct HyperSeq values each allow one iterator';

# The first .iterator succeeds and yields an Iterator.
{
    my $h = (^3).hyper;
    my $it = $h.iterator;
    ok $it.defined, 'first .iterator on HyperSeq returns a defined Iterator';
}

# Concurrent workers racing for the single iterator: exactly one wins.
{
    my $seq = (^10).hyper;
    my atomicint $succeeded = 0;
    my atomicint $thrown = 0;
    my $starter = Promise.new;
    my @w;
    for ^10 {
        @w.push: start {
            await $starter;
            my $rc = try $seq.iterator;
            with $rc {
                ++⚛$succeeded;
            } else {
                ++⚛$thrown if $! ~~ X::Seq::Consumed;
            }
        }
    }
    $starter.keep;
    await Promise.allof(@w);
    is $succeeded, 1, 'exactly one concurrent worker acquires the HyperSeq iterator';
    is $thrown, 9, 'the other nine workers get X::Seq::Consumed';
}
