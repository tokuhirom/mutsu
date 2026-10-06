use Test;
use lib 't/lib';
use MultiWhereScopeRole;

plan 3;

# From the ecosystem ASN::BER distribution (0.7.3, t/03-long-integers.t):
# `multi method parse($type where ASNSequenceOf)` names a role the module
# declares. Candidate matching ran the `where` in the caller's compunit, which
# never imported it, and died with "Undeclared name: ASNSequenceOf".
my $p = MWSParser.new;
is $p.parse(1), 'int', 'a candidate without a where still dispatches';
is $p.parse(MWSRole[Int].new), 'role', 'where naming a module role resolves from the caller';
is $p.parse(MWSPlain.new), 'plain', 'where naming a plain module role resolves too';
