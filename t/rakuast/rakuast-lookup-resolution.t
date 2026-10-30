use Test;

plan 12;

# Lookup nodes of the unit tree `$*CU` answers carry their resolution, as
# rakudo's CHECK phase resolves them (#15610): lexicals to their declaration,
# core operators and routines to `Declaration::External::Setting`, core types
# to `Declaration::External::Constant`. Read from a CHECK-time closure.
my %seen;
BEGIN {
    my $cu = $*CU;
    $cu.add-check-code({
        my sub walk($node) {
            if $node ~~ RakuAST::Var::Lexical && $node.name eq '$marker' {
                %seen<lexical> //= $node;
            }
            if $node ~~ RakuAST::Call::Name::WithoutParentheses && $node.name.canonicalize eq 'say' {
                %seen<say> //= $node;
            }
            if $node ~~ RakuAST::Postcircumfix::ArrayIndex {
                %seen<index> //= $node;
            }
            if $node ~~ RakuAST::Type::Simple && $node.name.canonicalize eq 'Int' {
                %seen<type> //= $node;
            }
            if $node ~~ RakuAST::Var::Dynamic {
                %seen<dynamic> //= $node;
            }
            if $node ~~ RakuAST::Var::Lexical && $node.name eq '$param' {
                %seen<param> //= $node;
            }
            $node.visit-children(&walk);
        }
        my $list = $cu.statement-list;
        walk($list);
        %seen<declaration> = $list.statements.grep(RakuAST::Statement::Expression).first({
            .expression ~~ RakuAST::VarDeclaration::Simple && .expression.desigilname.canonicalize eq 'marker'
        }).expression;
    });
}
my Int $marker = 1;
say $marker + 1 if False;
my @list = 1, 2;
@list[0] if False;
sub with-param($param) { $param }

ok %seen<lexical>.is-resolved, 'a lexical is resolved';
ok %seen<lexical>.resolution ~~ RakuAST::VarDeclaration::Simple, 'to its declaration';
is %seen<lexical>.resolution.compile-time-value.^name, 'Int', 'whose compile-time-value is its declared type';
ok %seen<lexical>.resolution === %seen<declaration>, 'which is the declaration of the tree';
ok %seen<param>.is-resolved && %seen<param>.resolution ~~ RakuAST::ParameterTarget::Var,
    'a parameter resolves to its target';
ok %seen<say>.is-resolved, 'a core routine call is resolved';
ok %seen<say>.resolution ~~ RakuAST::Declaration::External::Setting, 'to a setting declaration';
is %seen<say>.resolution.compile-time-value.^name, 'Sub', 'whose compile-time-value is the routine';
ok %seen<index>.resolution ~~ RakuAST::Declaration::External::Setting, 'a subscript resolves to the setting';
ok %seen<type>.resolution ~~ RakuAST::Declaration::External::Constant, 'a core type resolves to a constant';
is %seen<type>.resolution.compile-time-value.^name, 'Int', 'whose compile-time-value is the type object';
nok %seen<dynamic>.is-resolved, 'a dynamic variable is not resolved';
