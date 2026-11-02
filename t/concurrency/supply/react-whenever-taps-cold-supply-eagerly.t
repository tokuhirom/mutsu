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
is @log[0], 'block-start', 'cold supply body runs at whenever time';
is-deeply @log.grep(* ne "got 1").Array, ['block-start', 'after-whenever'],
    'the supply body starts before the rest of the react body';

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
is $port-p.status, Kept, 'supply body already ran when the react body continued';
