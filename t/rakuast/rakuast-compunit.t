use Test;

plan 21;

# `.AST(:compunit)` is a `RakuAST::CompUnit` around the statement list, and
# the in-place builders Needle::Compile uses to rewrite it.
my $cu = "say 42".AST(:compunit);
isa-ok $cu, RakuAST::CompUnit, '.AST(:compunit) is a CompUnit';
isa-ok $cu.statement-list, RakuAST::StatementList, 'its statement-list';
is $cu.comp-unit-name.chars, 40, 'a 40-character comp-unit-name';
isnt "say 42".AST(:compunit).comp-unit-name, $cu.comp-unit-name, 'each parse gets its own name';
isa-ok "say 42".AST(:!compunit), RakuAST::StatementList, ':!compunit is the plain statement list';

my sub stmt($expr) { RakuAST::Statement::Expression.new(expression => $expr) }
my sub lit($n) { RakuAST::IntLiteral.new($n) }

my $list := RakuAST::StatementList.new(stmt(lit(2)));
$list.unshift-statement(stmt(lit(1)));
$list.add-statement(stmt(lit(3)));
is $list.statements.map(*.expression.value).join(","), "1,2,3", 'unshift-statement prepends, add-statement appends';
is EVAL($list), 3, 'the rewritten list evaluates';

my $e := stmt(lit(1));
$e.set-expression(lit(5));
is $e.expression.value, 5, 'set-expression replaces the expression';
is EVAL($e), 5, 'and evaluates';

my $args := RakuAST::ArgList.new(RakuAST::StrLiteral.new("O"));
$args.push(RakuAST::ColonPair::True.new("i"));
is $args.args.elems, 2, 'ArgList.push appends an argument';
$_ = "fOo";
ok EVAL(RakuAST::Term::TopicCall.new(RakuAST::Call::Method.new(
  name => RakuAST::Name.from-identifier("contains"), args => $args))),
  'the pushed named argument reaches the call';

# A CompUnit evaluates, before and after its statement list is replaced.
my $unit = '$_ * 2'.AST(:compunit);
$unit.statement-list.unshift-statement(stmt(RakuAST::VarDeclaration::Simple.new(
  sigil => '$', twigil => '*', desigilname => RakuAST::Name.from-identifier('_'),
  initializer => RakuAST::Initializer::Bind.new(RakuAST::Var::Lexical.new('$_')))));
my $body := $unit.statement-list;
$unit.replace-statement-list(RakuAST::StatementList.new(stmt(RakuAST::PointyBlock.new(
  signature => RakuAST::Signature.new(parameters => (
    RakuAST::Parameter.new(target => RakuAST::ParameterTarget::Var.new(name => '$_')),)),
  body => RakuAST::Blockoid.new($body)))));
my &doubler = EVAL $unit;
is doubler(21), 42, 'a CompUnit whose statement list was replaced evaluates';

# `my $*x := ...` in both directions.
is Q|my $*x := $_|.AST.statements[0].expression.twigil, '*', 'a dynamic declaration carries its twigil';
$_ = 7;
is EVAL(Q|my $*x := $_; $*x|.AST), 7, 'a dynamic binding lowers';
sub reads-dyn { $*q }
is EVAL(Q|my $*q = 5; reads-dyn()|.AST), 5, 'a dynamic declaration is visible to a callee';
is EVAL(Q|my @*z = 1, 2; @*z.elems|.AST), 2, 'a dynamic array declaration';

# `Term::Name` for True/False is the Bool, not the string.
is-deeply EVAL(RakuAST::Term::Name.new(RakuAST::Name.from-identifier("False"))), False,
  'Term::Name False';
is-deeply EVAL(RakuAST::ApplyInfix.new(left => lit(0), infix => RakuAST::Infix.new("||"),
  right => RakuAST::Term::Name.new(RakuAST::Name.from-identifier("True")))), True,
  'Term::Name True inside an expression';

# A node with a role mixed in still lowers.
my role Tag { method tag { 'and' } }
my $tagged = lit(0) but Tag;
is $tagged.tag, 'and', 'the mixin is kept on the node';
is EVAL(RakuAST::ApplyInfix.new(left => $tagged, infix => RakuAST::Infix.new("||"),
  right => lit(6))), 6, 'a mixed-in node lowers';
is RakuAST::CompUnit.new(statement-list => RakuAST::StatementList.new, comp-unit-name => "x").comp-unit-name,
  "x", 'CompUnit.new';
