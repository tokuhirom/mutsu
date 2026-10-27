use lib 't/lib';
use Test;
use ProxyIncOps;

# Found by Red 0.2.7 (t/18-phasers): `Model.column++` where the column accessor
# is an `is rw` Proxy must reach the imported `postfix:<++>(Red::AST)`
# candidate; the Proxy operand was ranked unfetched, so the core operator ran.

plan 3;

class Holder {
    method col is rw {
        Proxy.new(FETCH => -> $ { Col.new(n => 7) }, STORE => -> $v { });
    }
}

Holder.col++;
is seen().elems, 1, 'the imported candidate ran for a Proxy accessor operand';
is seen()[0], 7, 'it received the FETCHed value';

my $c = Col.new(n => 3);
$c++;
is seen().elems, 2, 'a plain variable still reaches it';
