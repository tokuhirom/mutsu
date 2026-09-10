use Test;

plan 2;

# `done VALUE` is shorthand for emitting VALUE and then completing the supply.
my @got;
my $done = False;
supply { emit 1; done 20 + 22 }.tap(
    -> $value { @got.push($value) },
    done => { $done = True },
);

is @got, [1, 42], '`done VALUE` emits its value before completing';
ok $done, '`done VALUE` still completes the supply';
