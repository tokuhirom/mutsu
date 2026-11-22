use v6;
use Test;
use lib 't/lib';
use PunSubset18768::Base;
use PunSubset18768::Units;
use PunSubset18768::User;

plan 3;

# A role body run for the pun (here: a parameterization made inside a method
# of the role's pun) runs in the pun's package. A `my subset` it declares is
# qualified by the role's own name, not the pun's storage name, so marking the
# subset lexical does not make the role itself read as a lexical type of the
# module and vanish from the importer (#18768, CSS::Properties' CSS::Units).
is PunSubset18768::User.run, 'pt', 'the role is punned and parameterized from another compunit';
is PunSubset18768::Units.^name, 'PunSubset18768::Units', 'the role is still visible by name afterwards';
sub role-name { PunSubset18768::Units.^name }
is role-name(), 'PunSubset18768::Units', 'and from a routine';
