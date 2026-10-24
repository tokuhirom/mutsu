use v6;
use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 3;

# A method closure handed to `^add_multi_method` from a module keeps the
# module's own imports: the body calls a routine the module `use`d, not the
# program that finally calls the method.
my $dir = make-temp-dir;
$dir.add("AmmHelper.rakumod").spurt: q:to/END/;
    unit module AmmHelper;
    multi amm-val(Str $v) is export { "str:$v" }
    multi amm-val($v, :$type = $v.WHAT) is export { "any:$v" }
    END
$dir.add("AmmAdder.rakumod").spurt: q:to/END/;
    use AmmHelper;
    unit class AmmAdder;
    method add(Mu:U \type) {
        type.^add_multi_method: 'plain', method (::T:U:) { amm-val(1) }
        type.^add_multi_method: 'prox', method (::T:U:) is rw {
            Proxy.new:
                FETCH => -> $ { 0 },
                STORE => -> $, $value {
                    my $val = do given $value {
                        when Str { "s" }
                        default { amm-val $_, :type(Int) }
                    }
                    $*SEEN = $val;
                }
        }
    }
    END

sub out-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', $dir.Str, '-e', $code, :out, :err;
    $proc.out.slurp(:close) ~ $proc.err.slurp(:close)
}

my $setup = 'use AmmAdder; class Foo { }; AmmAdder.new.add(Foo); Foo.^compose;';
is out-of($setup ~ ' say Foo.plain'), "any:1\n", "a multi method added from a module calls the module's import";
is out-of($setup ~ ' my $*SEEN; Foo.prox = 42; say $*SEEN'), "any:42\n", "also from a Proxy STORE inside a given";
is out-of($setup ~ ' sub f { Foo.plain }; say f()'), "any:1\n", "and from inside another routine";
