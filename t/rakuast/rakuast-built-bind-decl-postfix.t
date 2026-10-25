use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# `is built(:bind)` keeps its argument across the RakuAST boundary.
my $attr = Q[class A { has $.e is built(:bind) }].AST.statements[0].expression.body.body.statement-list.statements[0].expression;
like $attr.traits[0].gist, /'ColonPair::True.new("bind")'/, 'the :bind argument is rendered';
my class B { has $.e is built(:bind); has $!f is built(:bind) }
is EVAL(Q[class C { has $.e is built(:bind); has $!f is built(:bind) }; C.^BUILDPLAN.map(*.[0]).join(",")].AST),
    B.^BUILDPLAN.map(*.[0]).join(","), 'the lowered class binds like the parsed one';

# A `.=` declaration used as a postfix operand is an expression-position declaration.
is EVAL(Q[(my Int $e .= new(4)).^name].AST), 'Int', 'a .= declaration under a postfix call';
is EVAL(Q[(my Int $e .= new(4)).WHAT.^name].AST), 'Int', 'its type object is the declared type';
done-testing;
