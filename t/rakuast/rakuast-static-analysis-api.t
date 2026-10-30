use Test;

plan 17;

# The RakuAST surface static::Analyzer reads from a CHECK-time closure (#15610):
# imported routines, a unit sub's compile-time-value, a parameter's
# meta-object and declaration, a declaration's return-type, an initializer's
# is-binding. Every answer is the one rakudo gives.
my %seen;
BEGIN {
    my $cu = $*CU;
    $cu.add-check-code({
        my sub walk($node) {
            if $node ~~ RakuAST::Call::Name && $node.name.canonicalize eq 'plan' {
                %seen<plan> //= $node;
            }
            if $node ~~ RakuAST::Call::Name && $node.name.canonicalize eq 'text' {
                %seen<text> //= $node;
            }
            if $node ~~ RakuAST::ParameterTarget::Var && $node.name eq '$typed' {
                %seen<typed> //= $node;
            }
            if $node ~~ RakuAST::VarDeclaration::Simple {
                %seen{'decl-' ~ $node.sigil ~ $node.desigilname.canonicalize} //= $node;
            }
            if $node ~~ RakuAST::Parameter && $node.target ~~ RakuAST::ParameterTarget::Var && $node.target.name eq '$typed' {
                %seen<param> //= $node;
            }
            $node.visit-children(&walk);
        }
        walk($cu.statement-list);
    });
}
sub text() { "wrong" }
sub with-typed(Int:D $typed) { $typed }
my Int $count = text().chars;
my @list;
my %map;
my $bound := $count;

ok %seen<plan>.is-resolved, 'an imported routine is resolved';
ok %seen<plan>.resolution ~~ RakuAST::Declaration::Import, 'to an import declaration';
is %seen<plan>.resolution.compile-time-value.^name, 'Sub', 'whose compile-time-value is the routine';
is %seen<text>.resolution.compile-time-value.^name, 'Sub', 'a unit sub has a compile-time-value';
ok %seen<text>.resolution.compile-time-value ~~ Routine, 'which is a Routine';
is %seen<param>.meta-object.^name, 'Parameter', 'a parameter has its meta-object';
is %seen<param>.meta-object.type.^name, 'Int', 'with the declared type';
is %seen<param>.meta-object.modifier, ':D', 'and its definiteness';
ok %seen<typed>.declaration ~~ RakuAST::VarDeclaration::Simple, 'a parameter target has its declaration';
ok %seen<typed>.declaration === %seen<typed>.declaration, 'which keeps its identity';
is %seen<typed>.declaration.desigilname.canonicalize, 'typed', 'and its name';
is %seen<decl-$count>.return-type.^name, 'Int', 'return-type of a typed scalar';
is %seen<decl-@list>.return-type.^name, 'Positional', 'of an array';
is %seen<decl-%map>.return-type.^name, 'Associative', 'of a hash';
nok %seen<decl-$count>.initializer.is-binding, 'an assignment is not a binding';
ok %seen<decl-$bound>.initializer.is-binding, 'a bind is';
is %seen<decl-$bound>.return-type.raku, 'Mu', 'return-type of an untyped scalar';
