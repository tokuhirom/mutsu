use Test;

plan 2;

my @results;

for 1..1 -> $iter {
    my $lock = Lock.new;
    my $condition = $lock.condition;
    my $done = 0;
    Thread.start({
        sleep 0.1;
        $lock.protect({
            $done = 1;
            $condition.signal;
        });
    });
    $lock.protect({ $condition.wait({ $done == 1 }) });
    @results.push($done);
}

while @results.elems < 2 {
    my $lock = Lock.new;
    my $condition = $lock.condition;
    my $done = 0;
    Thread.start({
        sleep 0.1;
        $lock.protect({
            $done = 1;
            $condition.signal;
        });
    });
    $lock.protect({ $condition.wait({ $done == 1 }) });
    @results.push($done);
}

is @results, [1, 1], 'loop-scoped Lock condition variables share captured state';
ok True, 'for and while loop bodies complete after the signal';

done-testing;
