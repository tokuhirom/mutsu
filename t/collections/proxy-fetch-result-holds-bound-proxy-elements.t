use Test;

# CSS::Properties' box accessors return a Proxy whose FETCH hands back an Array
# built with `@bound[$n++] := proxy`. Rendering the result of the call must read
# the elements through their containers (#14556).

plan 6;

sub item is rw {
    my $x = 5;
    Proxy.new(FETCH => sub ($) { $x }, STORE => sub ($, $n) { $x = $n });
}
sub box is rw {
    my @b;
    @b[0] := item();
    @b[1] := item();
    Proxy.new(FETCH => sub ($) { @b }, STORE => sub ($, $x) { });
}

is box().raku, '$[5, 5]', '.raku of the call result';
is box().gist, '[5 5]', '.gist';
is box().Str, '5 5', '.Str';
is box().join('+'), '5+5', '.join';
is box().list.raku, '[5, 5]', '.list';
is box().elems, 2, '.elems';
