use Test;

# Kind::Subset::Parametric (#15271): `Type.HOW.^mixin: Role` changes that one
# metaobject, so the role's metamethods answer through `Type.^method` and the
# role's attributes are shared state of the metaobject.

plan 12;

role Meta {
    has &!body;
    has $!n;
    method body(::?CLASS:D: Mu --> Callable:D) { &!body }
    method set-body(::?CLASS:D: Mu, &b --> Callable:D) { &!body := &b<> }
    method n(Mu) { $!n }
    method set-n(Mu, $v) { $!n = $v }
}

class Plain { }
Plain.HOW.^mixin: Meta;
Plain.^set-n(9);
is Plain.^n, 9, 'class: a metamethod of the mixed-in role is reachable via .^';
is Plain.HOW.n(Plain), 9, 'class: role attribute state is kept on the metaobject';
Plain.^set-body: { 43 };
is Plain.^body.(), 43, 'class: a code attribute bound in a role metamethod is kept';
like Plain.HOW.^name, /'ClassHOW+{Meta}'/, 'class: the HOW is the mixed-in type';

subset Small of Int where * < 10;
my $before = Small.HOW;
Small.HOW.^mixin: Meta;
like Small.HOW.^name, /'SubsetHOW+{Meta}'/, 'subset: the HOW is the mixed-in type';
ok Small.HOW ~~ Metamodel::SubsetHOW, 'subset: it is still a SubsetHOW';
Small.^set-body: { 42 };
is Small.^body.(), 42, 'subset: a code attribute survives to a later .^ call';
is Small.HOW.body(Small).(), 42, 'subset: and to a direct HOW call';
is Small.^refinee.^name, 'Int', 'subset: the original refinee is kept';
ok 3 ~~ Small && 30 !~~ Small, 'subset: the where clause still applies';

# Metamodel::Primitives.parameterize_type takes the parameters as a slurpy list.
package Cache {
    our sub direct(*@a) is raw { Metamodel::Primitives.parameterize_type: $?PACKAGE, |@a }
    our sub listed(+args) is raw { Metamodel::Primitives.parameterize_type: $?PACKAGE, args }
    sub make(Mu, Mu \args) is raw { args.elems ~ ":" ~ args.raku }
    BEGIN Metamodel::Primitives.set_parameterizer: $?PACKAGE, &make;
}
is Cache::direct(1, 2, 3), '3:(1, 2, 3)', 'parameterize_type: flattened positionals form the list';
is Cache::listed(1, 2, 3), '3:(1, 2, 3)', 'parameterize_type: one list argument is the list';
