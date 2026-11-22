use Test;

# An EVAL's setting is the lexical scope it runs in: `&SETTING::not` inside
# an EVAL sees a lexical `sub not`, and `&CALLER::SETTING::not` from a frame
# an EVAL compiled does too. Outside an EVAL, SETTING is the core setting.
# Roast S02-names/pseudo-6d.t 148-151 (#18742).

plan 8;

{
    sub not($x) { $x } #OK
    my $setting = 'SETTING';

    ok &SETTING::not(False), 'SETTING:: outside EVAL is the core setting';
    ok &::($setting)::not.(False), '::("SETTING") outside EVAL too';

    ok EVAL('&SETTING::not(True)'), 'SETTING:: inside EVAL finds the lexical sub';
    ok EVAL('&::($setting)::not(True)'), '::("SETTING") inside EVAL too';

    my $f = EVAL('-> $fn { $fn(); }');
    ok $f({ &CALLER::SETTING::not(True) }), 'CALLER::SETTING:: from an EVAL frame';
    ok $f({ &CALLER::($setting)::not(True) }), 'CALLER::($setting):: from an EVAL frame';

    my $g = -> $fn { $fn() };
    nok $g({ &CALLER::SETTING::not(True) }), 'CALLER::SETTING:: from a plain frame is core';
}

sub caller-pkg($fn) { $fn() }
is caller-pkg({ &CALLER::('CORE')::not(True) }), False,
    '&CALLER::($p)::f takes the CALLER prefix into account';
