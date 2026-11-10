use v6.e.PREVIEW;
use lib 't/lib';
use Test;
use nqp;

# A class a slang production installs resolves as a type in the unit's check
# code even when `$*CU` was read before the production ran (#13370): Qwiratry's
# `aggsub Tally` followed by `Tally.({ ... })` is rewritten at CHECK time only
# if `Tally` resolves.

plan 3;

use GeneratedClassSlang;

our @seen;
BEGIN {
    my $phasers := nqp::getattr($*CU, RakuAST::CompUnit, '$!check-phasers');
    nqp::push($phasers, {
        sub visit($n) {
            if nqp::istype($n, RakuAST::ApplyPostfix) && nqp::istype($n.postfix, RakuAST::Call::Term) {
                my $o = $n.operand;
                if (try $o.DEPARSE) eq 'Tally' {
                    @seen.push: $o.is-resolved;
                    @seen.push: $o.resolution.compile-time-value.^name;
                }
            }
            $n.visit-children(&visit) if nqp::can($n, 'visit-children');
        }
        visit($*CU);
    });
}

defclass Tally 5;
my $x = try Tally.({ 1 });

is @seen.elems, 2, 'the check code found the Tally application';
ok @seen[0], 'Tally is resolved in the check code';
is @seen[1], 'Tally', 'its compile-time value is the minted class';
