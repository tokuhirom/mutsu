use v6;
use lib 't/lib';
use Test;

# From Mathematica::Serializer: a module that `need`s a dependency exporting
# `our constant X is export` and re-exports its own `X` must still import `X`
# into its user. The sigilless import is recorded under its term key, which the
# ADR-11136 name-visibility gate did not consult, so the name stayed hidden.

plan 3;

use NeedDepExportConst;

is NeedDepSym.^name, 'NeedDepExportConstDep::Sym', 'bare exported constant resolves';
isa-ok NeedDepSym.new(name => 'Plot'), NeedDepSym, 'instance is an instance of the constant';
is NeedDepSym.new(name => 'Plot').name, 'Plot', 'constructed through the constant';
