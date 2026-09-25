use v6;
use Test;

# `++$x`, `$x++`, `--$x` and `$x--` on a named scalar are one operation with
# two knobs: the step direction and whether the old or the new value is the
# result. They used to have four separate bodies, and only `$x++` knew about
# `$CALLER::x` or a Proxy (#9450).

plan 16;

# -- $CALLER::x ------------------------------------------------------------
{
    sub post-inc { $CALLER::x++ }
    sub pre-inc  { ++$CALLER::x }
    sub post-dec { $CALLER::x-- }
    sub pre-dec  { --$CALLER::x }

    my $x is dynamic = 10;
    is post-inc(), 10, '$CALLER::x++ returns the old value';
    is $x, 11, '... and increments the caller variable';
    is pre-inc(), 12, '++$CALLER::x returns the new value';
    is $x, 12, '... and increments the caller variable';
    is post-dec(), 12, '$CALLER::x-- returns the old value';
    is $x, 11, '... and decrements the caller variable';
    is pre-dec(), 10, '--$CALLER::x returns the new value';
    is $x, 10, '... and decrements the caller variable';
}

# -- Proxy -----------------------------------------------------------------
{
    my $store = 5;
    my $p := Proxy.new(FETCH => -> $ { $store }, STORE => -> $, $v { $store = $v });
    is $p++, 5, 'postfix ++ on a Proxy returns the fetched value';
    is $store, 6, '... and STOREs the incremented one';
    is ++$p, 7, 'prefix ++ on a Proxy returns the new value';
    is $p--, 7, 'postfix -- on a Proxy returns the fetched value';
    is $store, 6, '... and STOREs the decremented one';
    is --$p, 5, 'prefix -- on a Proxy returns the new value';
}

# -- a readonly operand names the operator actually used --------------------
{
    sub bump($n) { $n-- }
    throws-like { bump(1) }, Exception, message => /'postfix:<-->'/,
        '$n-- on a readonly parameter reports postfix:<-->';
    sub bump2($n) { --$n }
    throws-like { bump2(1) }, Exception, message => /'prefix:<-->'/,
        '--$n on a readonly parameter reports prefix:<-->';
}
