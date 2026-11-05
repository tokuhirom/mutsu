use v6;
use Test;
use nqp;

# From CoreHackers::NfaChainsaw: its NfaRunner dispatches NFA edges with
# `when nqp::const::EDGE_FATE { ... }`. The QRegex::NFA edge constants must
# exist (values as rakudo's `nqp::const::EDGE_*`), and a `nqp::const::` name
# after `when` is a complete term, not a routine gobbling the block.

plan 5;

is (nqp::const::EDGE_FATE, nqp::const::EDGE_EPSILON, nqp::const::EDGE_CODEPOINT).join(','),
    '0,1,2', 'basic edge types';
is (nqp::const::EDGE_SUBRULE, nqp::const::EDGE_GENERIC_VAR, nqp::const::EDGE_CODEPOINT_LL).join(','),
    '8,11,14', 'subrule, generic var and LL edge types';
is (nqp::const::EDGE_CODEPOINT_I_LL, nqp::const::EDGE_CODEPOINT_M_LL, nqp::const::EDGE_CODEPOINT_IM_LL).join(','),
    '15,18,21', 'case/mark-insensitive LL edge types';
is (nqp::const::EDGE_CHARRANGE_M, nqp::const::EDGE_CHARRANGE_M_NEG).join(','),
    '22,23', 'charrange mark edges follow MoarVM numbering';

sub name-of($edge) {
    do given $edge {
        when nqp::const::EDGE_FATE    { 'fate' }
        when nqp::const::EDGE_EPSILON { 'epsilon' }
        default                       { 'other' }
    }
}
is (0, 1, 5).map(&name-of).join(','), 'fate,epsilon,other', 'nqp::const:: after when';
