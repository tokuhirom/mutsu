use Test;

# A Proxy FETCH runs as an ordinary closure call (#9385): its body sees its
# own lexical captures (the live value the STORE side mutates), dynamic
# variables resolve through the caller chain, and the read leaks nothing into
# the reading frame.

plan 7;

{
    my $store = 1;
    my $p := Proxy.new(
        FETCH => -> $ { $store * 10 },
        STORE => -> $, $v { $store = $v },
    );
    is $p, 10, 'FETCH reads the captured lexical';
    $p = 4;
    is $p, 40, 'FETCH sees the value STORE wrote to the shared capture';
    $store = 7;
    is $p, 70, 'FETCH sees a direct write to the captured lexical';
}

{
    my $x = 'captured';
    my $p := Proxy.new(FETCH => -> $ { $x }, STORE => -> $, $ { });
    sub reader($proxy is raw) { my $x = 'caller'; my $v = $proxy; $v }
    is reader($p), 'captured', 'a same-named caller lexical does not shadow the capture';
}

{
    my $p := Proxy.new(FETCH => -> $ { $*PROXY-DYN }, STORE => -> $, $ { });
    sub dyn-reader($proxy is raw) { my $*PROXY-DYN = 'dynamic'; my $v = $proxy; $v }
    is dyn-reader($p), 'dynamic', 'a dynamic variable resolves through the caller chain';
}

{
    my @proxies = (1, 2, 3).map: -> $v {
        Proxy.new(FETCH => -> $ { $v }, STORE => -> $, $ { })
    };
    is @proxies.map({ my $x := $_; $x + 0 }).join(','), '1,2,3',
        'Proxies sharing a captured name each read their own capture';
}

{
    my $n = 0;
    my $p := Proxy.new(FETCH => -> $ { ++$n }, STORE => -> $, $ { });
    my $a = $p;
    my $b = $p;
    is "$a $b", '1 2', 'every read runs FETCH once';
}
