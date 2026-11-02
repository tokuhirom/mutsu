use Test;

plan 3;

# Rakudo taps a `whenever`'s source the moment the `whenever` runs, so a cold
# `supply { }` body starts before the rest of the react body (#15958).
my @log;
my $s = supply { @log.push('block-start'); emit 1 };
react {
    whenever $s -> $v { @log.push("got $v"); done }
    @log.push('after-whenever');
}
is-deeply @log, ['block-start', 'after-whenever', 'got 1'],
    'cold supply body runs at whenever time, its emit is handled after the react body';

# A listener bound inside the supply body is accepting by the time the react
# body has finished, even when the body does slow work first.
my $ready = Promise.new;
my $port-p = Promise.new;
start react {
    my $src = supply {
        my $x = 0;
        $x++ for ^500_000;
        whenever IO::Socket::Async.listen('localhost', 0) -> $c { $c.close }
        $port-p.keep(True);
    };
    whenever $src -> $ { }
    $ready.keep(True);
}
await $ready;
is await($port-p), True, 'supply body already ran when the react body continued';

my @order;
my $t = supply { @order.push('body'); emit 'x' };
react {
    whenever $t { @order.push('handler'); done }
    @order.push('tail');
}
is-deeply @order, ['body', 'tail', 'handler'], 'plain emit is delivered after the react body';
