use Test;
use experimental :rakuast;

plan 25;

sub mk-sub(Str $name = "foo") {
    RakuAST::Sub.new(
        name => RakuAST::Name.from-identifier($name),
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new),
    )
}

sub names($node) {
    my @n;
    $node.visit-children({ @n.push(.^name.split('::').tail) });
    @n
}

# A Doc::Declarator needs no WHEREFORE to be built.
my $d = RakuAST::Doc::Declarator.new(:leading(["a"]));
is $d.leading.raku, '("a",)', 'leading of a doc built without WHEREFORE';
is $d.trailing.raku, '()', 'trailing defaults to an empty list';

$d.add-leading("b");
$d.add-trailing("t");
is $d.leading.raku, '("a", "b")', 'add-leading appends a line';
is $d.trailing.raku, '("t",)', 'add-trailing appends a line';
$d.set-leading(["x"]);
is $d.leading.raku, '("x",)', 'set-leading replaces the lines';
$d.set-trailing(["y", "z"]);
is $d.trailing.raku, '("y", "z")', 'set-trailing replaces the lines';
is $d.paragraphs.raku, '("x", "y\nz")', 'paragraphs joins each part by a newline';

# declarator-docs / cut-WHY on a declaration.
my $s = mk-sub;
is $s.WHY.^name, 'RakuAST::Doc::Declarator', 'an undocumented declaration answers the type object';
ok !$s.WHY.defined, '... which is not defined';
is ($s.declarator-docs(leading => ["L1", "L2"], trailing => ["T"]) === $s).so, True,
    'declarator-docs answers the invocant';
is $s.WHY.leading.raku, '("L1", "L2")', 'declarator-docs sets the leading lines';
is $s.WHY.trailing.raku, '("T",)', 'declarator-docs sets the trailing lines';
$s.declarator-docs(leading => ["Z"]);
is $s.WHY.trailing.raku, '()', 'a second declarator-docs replaces both parts';
my $cut = $s.cut-WHY;
is $cut.leading.raku, '("Z",)', 'cut-WHY answers the documentation';
ok !$s.WHY.defined, '... and the declaration holds none afterwards';

my $m = RakuAST::Method.new(
    name => RakuAST::Name.from-identifier("m"),
    body => RakuAST::Blockoid.new(RakuAST::StatementList.new),
);
$m.declarator-docs(leading => ["ml"]);
is $m.WHY.leading.raku, '("ml",)', 'declarator-docs on a Method';

# set-WHY with a doc that names the declaration itself must not make the
# tree contain itself.
my $t = mk-sub("bar");
$t.set-WHY(RakuAST::Doc::Declarator.new(:leading(["lead"]), :WHEREFORE($t)));
is $t.WHY.leading.raku, '("lead",)', 'set-WHY keeps a doc whose WHEREFORE is the declaration';
is $t.WHY.WHEREFORE.^name, 'RakuAST::Sub', '... and WHEREFORE answers the declaration';

# EVAL of a hand-built documented declaration: `&NAME.WHY.PART` run after it.
sub why-of($sub, Str $name, Str $part) {
    my $call = RakuAST::ApplyPostfix.new(
        operand => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new("&$name"),
            postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('WHY'))),
        postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier($part)));
    EVAL RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => $sub),
        RakuAST::Statement::Expression.new(expression => $call))
}
my $u = mk-sub("baz");
$u.declarator-docs(leading => ["a", "b"], trailing => ["c"]);
is why-of($u, "baz", "leading"), "a b", 'EVAL lowers declarator-docs: leading';
is why-of($u, "baz", "trailing"), "c", 'EVAL lowers declarator-docs: trailing';
is why-of($t, "bar", "leading"), "lead", 'EVAL lowers a set-WHY doc';

# visit-children visits the documentation.
my $parsed = "#| hello\nsub foo \{ 1 \}".AST.statements[0].expression;
is names($parsed).join(' '), 'Name Declarator Signature Blockoid',
    'a documented sub visits its Doc::Declarator after the name';
my $anon = "my \$f = #| anon\n sub \{ 1 \}".AST.statements[0].expression.initializer.expression;
is names($anon).join(' '), 'Declarator Signature Blockoid',
    'an anonymous documented sub visits it first';
my $class = "#| c\nclass C \{ \}".AST.statements[0].expression;
is names($class).join(' '), 'Name Block Declarator', 'a documented class visits it last';
is names("sub foo \{ 1 \}".AST.statements[0].expression).join(' '), 'Name Signature Blockoid',
    'an undocumented sub visits no doc';
