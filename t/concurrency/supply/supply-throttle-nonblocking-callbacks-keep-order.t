use Test;

# Callbacks that never block run one after another in input order even though
# throttle may run several at once; only a callback that blocks lets its
# siblings start (roast S17-channel/basic.t "got all urls").

plan 3;

my @seen;
my $lock = Lock.new;
await (1..40).Supply.throttle: 4, { $lock.protect: { @seen.push($_) } };
is-deeply @seen, [1..40], 'non-blocking callbacks run in input order';

my @order;
my $l2 = Lock.new;
await (1..6).Supply.throttle: 3, -> $n {
    $l2.protect: { @order.push("start $n") };
    sleep 0.2 if $n == 1;
    $l2.protect: { @order.push("end $n") };
};
ok @order.first(* eq 'end 1').defined, 'the blocking callback finished';
ok @order.first(* eq 'end 1', :k) > @order.first(* eq 'start 2', :k),
    'a blocking callback lets the next one start before it ends';
