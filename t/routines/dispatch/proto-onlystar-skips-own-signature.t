use Test;

plan 4;

# A proto whose body is just `{*}` does not bind its own signature in Rakudo:
# the arguments go straight to the candidates (#13250).
class W {
    proto method wn(Numeric:D $, $? --> Str) {*}
    multi method wn(Numeric:D $n, $u) { "num" }
    multi method wn(@t [ % ( :%func! ) ], $u) { "calc" }
}
is W.new.wn(5, 'em'), 'num', 'method proto: a candidate the proto accepts';
is W.new.wn([{ :func({ :ident<calc> }) },], 'em'), 'calc',
    'method proto: a candidate only the multi accepts';

proto sub ps(Int $x) {*}
multi sub ps(Int $x) { "i" }
multi sub ps(Str $x) { "s" }
my $v = "a";
is ps($v), 's', 'sub proto: runtime Str reaches the Str candidate';
is ps(5), 'i', 'sub proto: Int still reaches the Int candidate';
