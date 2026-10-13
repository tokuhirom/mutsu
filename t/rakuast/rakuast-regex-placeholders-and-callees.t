use v6;
use Test;

# More regex constructs whose tree was missing. Measured on rakudo 2026.09:
#   /$^a/                    Regex::Interpolation(var => VarDeclaration::Placeholder::Positional("$a"))
#   <&$tok(1)>               Assertion::Callable(callee => Contextualizer::Item(Var::Lexical("$tok")))
#   <@Pkg::list>             Assertion::InterpolatedVar(var => Var::Package(Name("Pkg", "list"), sigil "@"))
#   :my token T { \d }<NL>   Regex::Statement(TokenDeclaration(scope => "my")), with no WithWhitespace
#   $( rx/<["']>/ )          a nested regex's class quotes are members, not string quotes

plan 18;

sub body($src) { $src.AST.statements[0].expression.body }

# Read direction.
my $ph-block = Q[{ "abc" ~~ /$^a/ }].AST.statements[0].expression;
my $ph = $ph-block.body.statement-list.statements[0].expression.right.body;
is $ph.^name, 'RakuAST::Regex::Interpolation', '$^a in a regex is an interpolation';
is $ph.var.^name, 'RakuAST::VarDeclaration::Placeholder::Positional', '... of a placeholder';
my $call = body(Q[/<&$tok(1)>/]);
is $call.^name, 'RakuAST::Regex::Assertion::Callable', '<&$tok(1)> is a callable assertion';
is $call.callee.^name, 'RakuAST::Contextualizer::Item', '... whose callee is item-contextualized';
is $call.args.args.elems, 1, '... with its argument';
my $pkg = body(Q[/<@Foo::list>/]);
is $pkg.^name, 'RakuAST::Regex::Assertion::InterpolatedVar', '<@Foo::list> interpolates a variable';
is $pkg.var.^name, 'RakuAST::Var::Package', '... a package variable';
is $pkg.var.sigil, '@', '... with its sigil';
my $decl = body("/\n :my token T \{ \\d }\n <T> /");
is $decl.^name, 'RakuAST::Regex::Sequence', ':my token without a semicolon parses';
is $decl.terms[0].^name, 'RakuAST::Regex::Statement', '... as a statement with no whitespace wrapper';

# EVAL direction and semantics.
my &f = EVAL Q[my &g = { so 'abc' ~~ /$^a/ }; &g].AST;
is &f.arity, 1, 'EVAL of a block with /$^a/ takes the placeholder';
ok f('b'), '... and matches its argument';
my $tok = token ($p) { $p \d+ };
is ~('ab12' ~~ EVAL Q[/ <&$tok('ab')> /].AST), 'ab12', 'EVAL of <&$tok(arg)> calls the token';
is ~('ab12' ~~ EVAL Q[/ <&$tok: 'ab'> /].AST), 'ab12', 'EVAL of <&$tok: arg> calls the token';
package Letters { our @list = <a b> }
ok 'ab' ~~ EVAL(Q[/^ <@Letters::list>+ $/].AST), 'EVAL of <@Pkg::list> matches its elements';
nok 'ac' ~~ EVAL(Q[/^ <@Letters::list>+ $/].AST), '... and only them';
ok 'a1' ~~ EVAL("/\n :my token T \{ \\d }\n a <T>\n /".AST), 'EVAL of :my token';
ok 'ab' ~~ EVAL(Q[/ $( rx/ <["']>? a / ) b /].AST), 'EVAL of a nested rx class with quotes';
