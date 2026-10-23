use Test;

# Package declarations carry the origins the parser read them from
# (ADR-10723 Stage 2): the package, its name, and its block, blockoid and
# statement list. Ranges measured on rakudo 2026.09.

plan 16;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }

my $src = q:to/END/;
class A::B is Any {
    has $.x;
    also does Positional;
}
my role R[::T] { }
grammar G { token TOP { a } }
END
my @s = $src.AST.statements;

my $class = @s[0].expression;
is range($class), '0..60', 'a class';
is range($class.name), '6..10', 'its qualified name';
is range($class.body), '18..60', 'its block';
is range($class.body.body), '18..60', 'its blockoid';
is range($class.body.body.statement-list), '24..58',
    'its statement list, an `also` statement included';

my $role = @s[1].expression;
is range($role), '61..79', 'a role, its scope declarator included';
is range($role.name), '69..70', 'its name';
is range($role.body), '69..79', 'its block starts at the name';
is range($role.body.body), '76..79', 'its blockoid';
is range($role.body.body.statement-list), '77..77', 'an empty statement list';

my $grammar = @s[2].expression;
is range($grammar), '80..109', 'a grammar';
is range($grammar.name), '88..89', 'its name';
is range($grammar.body), '90..109', 'its block';
is range($grammar.body.body.statement-list), '92..107', 'its statement list';

# --- a hand-built or evaluated tree is unaffected --------------------------
is EVAL(q[class P { method m { 42 } }; P.m].AST), 42, 'the round trip still runs';
nok RakuAST::Class.new(name => RakuAST::Name.from-identifier('Q')).origin.defined,
    'a hand-built package has no origin';
