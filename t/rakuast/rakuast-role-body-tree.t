use experimental :rakuast;
use Test;

# The unit tree of a `role` as rakudo builds it, which static::Analyzer walks
# (#15610): the role's body holds its name and signature and is a routine, the
# statements are bracketed by the implicit `Role::ResolveInstantiations` and
# `nqp::list(MetaObject, nqp::curlexpad)` ones, and a private role method has a
# `Method` value. Expected values are rakudo 2026.09's.

plan 9;

role R { method !p() { 1 }; method v() { 2 } }

my @kinds;
my @bodies;
my @methods;
CHECK {
    sub walk($n) {
        return unless $n ~~ RakuAST::Node;
        @bodies.push: $n if $n ~~ RakuAST::RoleBody;
        @methods.push: $n if $n ~~ RakuAST::Method;
        $n.visit-children({ walk($_) });
    }
    walk($*CU);
    @bodies[0].visit-children({ @kinds.push: .^name });
}

is @bodies.elems, 1, 'one role body';
is @kinds.join(' '), 'RakuAST::Name RakuAST::Signature RakuAST::Blockoid', 'a body holds name, signature and block';
is @bodies[0].name.canonicalize, 'R', 'its name is the role\'s';
ok @bodies[0].signature ~~ RakuAST::Signature, 'it has a signature';
ok @bodies[0].compile-time-value ~~ Sub, 'a role body is a routine';
my @statements = @bodies[0].body.statement-list.statements;
is @statements.map(*.^name).join(' '),
    'RakuAST::Role::ResolveInstantiations RakuAST::Statement::Expression RakuAST::Statement::Expression RakuAST::Statement::Expression',
    'the implicit statements bracket the body';
is @statements[*-1].expression.args.map(*.^name).join(' '), 'RakuAST::Role::MetaObject RakuAST::Nqp', 'the closing nqp::list';
is @methods.elems, 2, 'both methods are in the tree';
ok @methods[0].compile-time-value ~~ Method, 'a private role method has a Method value';
