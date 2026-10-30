use experimental :rakuast;
use Test;

# What static::Analyzer reads of a unit's classes at CHECK time (#15610):
# `Method.compile-time-value` is the class's method object, a package
# declaration is a `Lookup` that nothing resolves, and `Infix.operator` is a
# method of the class. Expected values are rakudo 2026.09's.

plan 12;

unit module Example;

class C { method value() { 42 }; submethod BUILD() {}; multi method m(Int $a) { } }

my %seen;
sub walk($node) {
    return unless $node ~~ RakuAST::Node;
    given $node {
        when RakuAST::Method | RakuAST::Submethod {
            %seen{.name.canonicalize} = .compile-time-value;
        }
        when RakuAST::Package {
            %seen{'is-resolved ' ~ .^name.subst('RakuAST::', '')} = .is-resolved;
        }
    }
    $node.visit-children({ walk($_) });
}
CHECK { walk($*CU) }

is %seen<value>.^name, 'Method', 'a method answers its method object';
is %seen<value>.name, 'value', '... of that name';
is %seen<value>.package.^name, 'Example::C', '... found in the qualified class';
is %seen<BUILD>.^name, 'Submethod', 'a submethod answers a Submethod';
is %seen<m>.^name, 'Method', 'a multi method answers a Method';
is %seen{'is-resolved Module'}, False, 'a module declaration is not resolved';
is %seen{'is-resolved Class'}, False, 'a class declaration is not resolved';
ok RakuAST::Class ~~ RakuAST::Lookup, 'a package is a Lookup';
ok RakuAST::Class.^can('is-resolved'), '... with is-resolved';

ok RakuAST::Infix.^can('operator'), 'Infix can operator';
ok RakuAST::Postfix.^can('operator'), 'Postfix can operator';
ok RakuAST::IntLiteral.^can('value'), 'IntLiteral can value';
