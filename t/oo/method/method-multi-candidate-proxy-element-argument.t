use Test;

# A multi method candidate is chosen by what a Proxy argument FETCHes, also when
# the Proxy is an array element `:=`-bound to it (CSS::Box passes
# `$css.padding[0]` to `measure`, #14555).

plan 5;

my $store = 10;
sub mk is rw {
    my @b;
    @b[0] := Proxy.new(FETCH => sub ($) { $store }, STORE => sub ($, $v) { });
    @b;
}

class K {
    multi method m(Numeric $v) { "num $v" }
    multi method m(Str $v) { "str" }
    method s(Numeric $v) { "plain $v" }
}
my $k = K.new;

is $k.m(mk()[0]), 'num 10', 'multi method, element of a call result';
is $k.s(mk()[0]), 'plain 10', 'plain method';
my @b;
@b[0] := Proxy.new(FETCH => sub ($) { $store }, STORE => sub ($, $v) { });
is $k.m(@b[0]), 'num 10', 'multi method, element of a local array';
my $p = Proxy.new(FETCH => sub ($) { $store }, STORE => sub ($, $v) { });
is $k.m($p), 'num 10', 'a Proxy variable';

class W { has K $.k handles <m> = K.new }
$store = 7;
is W.new.m(@b[0]), 'num 7', 'through handles';
