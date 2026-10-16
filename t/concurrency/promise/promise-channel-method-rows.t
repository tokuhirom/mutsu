use Test;

# ADR-11276 §9.118: `Promise`'s, `Promise::Vow`'s and `Channel`'s methods are
# rows of the method table, found by their owner. Answers checked against
# Rakudo 2026.09.

plan 17;

my $p = Promise.new;
nok $p.Bool, 'a planned promise is False';
is $p.status, Planned, '.status';
my $v = $p.vow;
is $v.^name, 'Promise::Vow', '.vow answers the vow';
$v.keep(5);
is $p.result, 5, 'the vow kept the promise';
throws-like { $p.keep(1) }, X::Promise::Vowed, 'keep after the vow was taken';
ok $p.^can('vow'), '.^can sees Promise.vow';

my $b = Promise.new;
$b.break('oops');
is $b.cause.message, 'oops', '.cause of a broken promise';
is Promise.kept(3).then({ .result + 1 }).result, 4, '.then';
is Promise.kept(3).andthen({ .result * 2 }).result, 6, '.andthen';
is Promise.broken('x').orelse({ 'recovered' }).result, 'recovered', '.orelse';

my $c = Channel.new;
$c.send($_) for 1..3;
is $c.receive, 1, '.receive';
is $c.poll, 2, '.poll';
$c.close;
is $c.List, (3,), '.List drains the rest';
ok $c.closed.Bool, '.closed is kept once closed';
throws-like { $c.receive }, X::Channel::ReceiveOnClosed, 'receive on a closed, drained channel';
throws-like { Channel.new.elems }, Exception,
    message => 'Cannot determine number of elements on a Channel', '.elems';

my $d = Channel.new;
$d.send($_) for <a b>;
$d.close;
is $d.Array.raku, '["a", "b"]', '.Array drains into an Array';
