use Test;
use lib 't/lib-unit-class-child-package';

plan 5;

# `unit class UCCP;` followed by `use UCCP::Msg;` where the child module says
# `unit package UCCP;` re-opens the namespace; it must not demote the class to
# a bare package. Async::Workers does exactly this, and AttrX::Mooish's
# `trait_mod:<is>` reads `$*PACKAGE.HOW` while the class body is registering.
my @seen;
multi trait_mod:<is>(Attribute:D $attr, :$probe!) {
    @seen.push: $*PACKAGE.HOW.^name;
}

unit class UCCP;
use UCCP::Msg;

has $!x is probe;

is @seen.elems, 1, 'the attribute trait ran';
is @seen[0], 'Perl6::Metamodel::ClassHOW', '$*PACKAGE.HOW is the class HOW inside the body';
is UCCP.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'the class keeps its metaclass';
is UCCP.new.^name, 'UCCP', 'it still instantiates';
is UCCP::Msg.new.^name, 'UCCP::Msg', 'the child module class is reachable';

done-testing;
