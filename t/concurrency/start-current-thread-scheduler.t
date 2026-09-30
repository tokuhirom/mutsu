use Test;

plan 5;

{
    my $*SCHEDULER = CurrentThreadScheduler.new;
    my $n = 0;
    my $p = start { $n++; 42 };
    is $p.status, Kept, 'start under a CurrentThreadScheduler runs inline';
    is $n, 1, 'the body has already run when start returns';
    is $p.result, 42, 'the promise carries the body result';

    my $q = start { die "boom" };
    is $q.status, Broken, 'a dying body breaks the promise inline';
    is $q.cause.message, 'boom', 'the cause is the exception';
}
