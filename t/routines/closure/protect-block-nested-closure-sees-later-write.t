use Test;
plan 5;

for 1..1 {
    my $lock = Lock.new;
    my $n = 0;
    my $seen = $lock.protect({ my $h = -> { $n }; $n++; $h() });
    is $seen, 1, 'a closure made in a protect block sees the later write';
    is $n, 1, 'the enclosing lexical holds the write';
}

# A cue callback made inside protect, before the counter it reads is bumped.
for 1..3 -> $iter {
    my $lock = Lock.new;
    my $cond = $lock.condition;
    my ($todo, $done) = 0, 0;
    for 1..5 {
        $lock.protect({
            $*SCHEDULER.cue({
                $lock.protect({
                    $done++;
                    $cond.signal if $done == $todo;
                });
            });
            $todo++;
        });
    }
    $lock.protect({ $cond.wait unless $done == $todo; });
    is $done, 5, "all cue callbacks ran (iteration $iter)" if $iter == 3;
}

{
    my $lock = Lock.new;
    my $x = 0;
    $lock.protect({ $x = 3; });
    is $x, 3, 'a plain write in a protect block still reaches the caller';
}
{
    my $lock = Lock.new;
    my $x = 1;
    is $lock.protect({ my $c = -> { $x }; $x = 5; $c() }), 5, 'bare-block variant';
}
