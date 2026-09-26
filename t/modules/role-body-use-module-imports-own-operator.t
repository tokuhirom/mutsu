use lib 't/lib';
use Test;

# A module first loaded by a `use` inside a role body imports into ITSELF for
# its own nested `use`s. The role body's import target (the role's package)
# stayed set while the module's own body ran, so the module's `use Units :pt`
# installed `postfix:<pt>` into the role and the module's own `10pt` died with
# "Bogus postfix: pt" (CSS::Properties::Calculator, loaded from the
# CSS::TagSet role's body).

plan 2;

role R {
    use RoleBodyLoadOp::Calc;
    method size  { $RoleBodyLoadOp::Calc::size }
    method small { RoleBodyLoadOp::Calc::small() }
}
class X does R { }

is X.size, '10pt', 'a module-level initializer sees its own imported operator';
is X.small, '6pt', 'so does a compile-time constant';
