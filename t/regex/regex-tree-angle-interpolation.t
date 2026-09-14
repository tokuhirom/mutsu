use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: indirect aggregate assertions retain their
# RakuAST::Regex::Assertion::InterpolatedVar shape while the established
# runtime parser keeps their match-time value semantics.

plan 22;

my @parts = <a b>;
my $aggregate = /<@parts>/;
ok 'a' ~~ $aggregate, 'an angle array assertion matches its first element';
ok 'b' ~~ $aggregate, 'an angle array assertion matches its second element';
nok 'c' ~~ $aggregate, 'an angle array assertion rejects a missing element';
@parts = <c>;
ok 'c' ~~ $aggregate, 'an angle array assertion observes reassignment';
nok 'a' ~~ $aggregate, 'an angle array assertion does not retain old elements';

is Q[/<@parts>/].AST.gist, q:to/END/.chomp, 'an angle array assertion retains its aggregate RakuAST shape';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::InterpolatedVar.new(
        sequential => False,
        var        => RakuAST::Var::Lexical.new("\@parts")
      )
    )
  )
)
END

my $node = Q[/<@parts>/].AST.statements[0].expression.body;
ok $node ~~ RakuAST::Regex::Assertion::InterpolatedVar,
    'the angle array assertion has its dedicated node type';
is $node.var.name, '@parts', 'the angle array assertion retains its sigil';
is $node.sequential, False, 'an ordinary angle array assertion is not sequential';

my $sequential = Q[/x || <@parts>/].AST.statements[0].expression.body;
ok $sequential ~~ RakuAST::Regex::SequentialAlternation,
    'an angle array assertion composes with sequential alternation';
my $sequential-node = $sequential.branches[1];
ok $sequential-node ~~ RakuAST::Regex::Assertion::InterpolatedVar,
    'the sequential branch retains the angle array assertion';
is $sequential-node.sequential, True,
    'the angle array assertion records sequential context';

@parts = <bar baz>;
my $lookaround = /foo <?before <@parts>>/;
ok 'foobar' ~~ $lookaround,
    'an angle array assertion works inside a lookaround';
@parts = <qux>;
ok 'fooqux' ~~ $lookaround,
    'a lookaround angle array assertion observes reassignment';
nok 'foobar' ~~ $lookaround,
    'a lookaround angle array assertion does not retain old elements';

my $constructed-node = RakuAST::Regex::Assertion::InterpolatedVar.new(
    sequential => False,
    var => RakuAST::Var::Lexical.new(q[@parts]),
);
ok $constructed-node ~~ RakuAST::Regex::Assertion::InterpolatedVar,
    'the constructed aggregate assertion uses the measured node type';
is $constructed-node.var.name, '@parts',
    'the constructed aggregate assertion exposes its sigil';
my $constructed = EVAL(RakuAST::QuotedRegex.new(body => $constructed-node));
ok 'fooqux' ~~ $constructed,
    'a constructed angle array assertion lowers through the existing path';
@parts = <emu>;
ok 'emu' ~~ $constructed,
    'a constructed angle array assertion keeps match-time binding';

my %labels = a => '';
is Q[/<%labels>/].AST.gist, q:to/END/.chomp, 'an angle hash assertion retains the same measured model class';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::InterpolatedVar.new(
        sequential => False,
        var        => RakuAST::Var::Lexical.new("\%labels")
      )
    )
  )
)
END
my $hash-node = Q[/<%labels>/].AST.statements[0].expression.body;
ok $hash-node ~~ RakuAST::Regex::Assertion::InterpolatedVar,
    'the angle hash assertion has its dedicated node type';
is $hash-node.var.name, '%labels', 'the angle hash assertion retains its sigil';
