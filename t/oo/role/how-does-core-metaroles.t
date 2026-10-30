use Test;
use nqp;

plan 12;

# The metaroles Rakudo composes into its built-in meta-objects: a type's HOW
# answers a type check against them (the `Kind` module's refinement checks
# `nqp::istype_nd($type.HOW, Metamodel::...)`).
is nqp::istype_nd(Mu.HOW, Metamodel::AttributeContainer), 1, 'a class HOW does AttributeContainer';
is nqp::istype_nd(Int.HOW, Metamodel::Naming), 1, 'a class HOW does Naming';
is nqp::istype_nd(Str.HOW, Metamodel::REPRComposeProtocol), 1, 'a class HOW does REPRComposeProtocol';
is nqp::istype_nd(Array.HOW, Metamodel::Stashing), 1, 'a class HOW does Stashing';
is nqp::istype_nd(Mu.HOW, Metamodel::Versioning), 1, 'a class HOW does Versioning';
is nqp::istype_nd(Mu.HOW, Metamodel::TypePretense), 0, 'a class HOW does not do TypePretense';

module M { }
is nqp::istype_nd(M.HOW, Metamodel::Versioning), 1, 'a module HOW does Versioning';
is nqp::istype_nd(M.HOW, Metamodel::AttributeContainer), 0, 'a module HOW has no attributes';

role R { }
is nqp::istype_nd(R.HOW, Metamodel::RolePunning), 1, 'a role group HOW does RolePunning';
is nqp::istype_nd(R.HOW, Metamodel::MethodContainer), 0, 'a role group HOW holds no methods';

grammar G { }
is nqp::istype_nd(G.HOW, Metamodel::Mixins), 1, 'a grammar HOW is a class HOW';

my $block = { nqp::hllbool(nqp::can($_, 'parameterize')) };
ok $block(Blob.HOW), 'the HOW of a parametric role group can parameterize';
