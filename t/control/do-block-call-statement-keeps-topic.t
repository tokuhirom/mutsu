use Test;

# #15955: a routine called for the first time as a statement inside a `do { }`
# block must not leave the callee's frame root in place of the caller's env.
# The callee's env (a `scoped_child` that wrote nothing) shares the process-wide
# empty overlay with the caller's block tier, so a restore that compared the
# overlays only skipped the restore and the enclosing `for` topic read `Any`.

plan 9;

sub give { 42 }
sub noop { }

for <x> { do { give(); is $_, 'x', 'a first sub-call statement in a do block keeps the for topic' } }
for <x> { do { give(); is $_, 'x', 'and again once the sub has been called' } }

sub first-call { 1 }
for <y> {
    my $r = do { first-call(); $_ };
    is $r, 'y', 'a first call before the do block yields the topic';
}

sub other-noop { }
for <z> { do { other-noop(); is $_.raku, '"z"', 'a method call on the topic after the call' } }

sub third { }
given 'g' { do { third(); is $_, 'g', 'given topic' } }

sub fourth { 2 }
for <p q> { do { fourth(); is $_.uc, $_.uc, 'every iteration' } }

{
    sub fifth { }
    my @seen;
    for <a b> { do { fifth(); @seen.push($_) } }
    is-deeply @seen, ['a', 'b'], 'the topic of each iteration is kept';
}

sub with-arg($x) { $x }
for <m> { do { with-arg(1); is $_, 'm', 'a call with an argument' } }
