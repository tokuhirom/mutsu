use v6.d;
use Test;

# Test::Run (t/01-test-run.rakutest): `:exitcode([$exitcode = 0, :pass($p) = True])`
# with no `:exitcode` passed destructures an empty list, so the inner defaults
# apply.

plan 6;

sub t(:out([$o?, :pass($op) = True]), :exitcode([$e = 0, :pass($ep) = True])) {
    "o={$o.raku} op={$op.raku} e={$e.raku} ep={$ep.raku}"
}
is t(), 'o=Any op=Bool::True e=0 ep=Bool::True', 'nothing passed: inner defaults apply';
is t(:exitcode[5, :!pass]), 'o=Any op=Bool::True e=5 ep=Bool::False', 'supplied value wins';
is t(:out["x"]), 'o="x" op=Bool::True e=0 ep=Bool::True', 'the other unpack param still defaults';

sub u(:x([$a = 7])) { $a }
is u(), 7, 'single positional default';
is u(:x[3]), 3, 'supplied';

sub w(:x([$a, $b = 2])) { "$a $b" }
is w(:x[1]), '1 2', 'defaulted second element';
