use Test;

plan 2;

my $x = 1;
class Store {
    has &.now is required;
    method read() { &!now() }
}
my $store = Store.new(now => { $x });
my $cmd = Channel.new;
my $out = Channel.new;
my $w = start {
    for ^2 { $cmd.receive; $out.send($store.read()) }
}
$cmd.send(1);
is $out.receive, 1, 'closure-literal named method arg sees initial value';
$x = 42;
$cmd.send(1);
is $out.receive, 42,
    'closure-literal named method arg observes later mutation across threads';
await $w;

done-testing;
