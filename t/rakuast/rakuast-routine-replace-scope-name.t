use v6.e.PREVIEW;
use Test;

# `Declaration.replace-scope` and `Routine.replace-name` rewrite a routine node in
# place (#13370: Qwiratry's `mold` declarator turns the method it opened into a
# `my` sub named `_q_mold_NAME`).

plan 12;

my $m = RakuAST::Method.new(name => RakuAST::Name.from-identifier("foo"));
is $m.scope, 'has', 'a method defaults to the has scope';
nok $m.replace-scope('my').defined, 'replace-scope answers Any';
is $m.scope, 'my', 'the scope changed';
is $m.name.canonicalize, 'foo', 'the name is unchanged';

$m.replace-name(RakuAST::Name.from-identifier("bar"));
is $m.name.canonicalize, 'bar', 'replace-name changed the name';
is $m.raku, q:to/END/.chomp, 'both show in the constructor form, scope first';
    RakuAST::Method.new(
      scope => "my",
      name  => RakuAST::Name.from-identifier("bar"),
      body  => RakuAST::Blockoid.new(
        RakuAST::StatementList.new()
      )
    )
    END

my $s = RakuAST::Sub.new(name => RakuAST::Name.from-identifier("x"));
is $s.scope, 'my', 'a sub defaults to the my scope';
$s.replace-scope('our');
is $s.scope, 'our', 'a sub takes a new scope';

my $anon = RakuAST::Sub.new;
$anon.replace-name(RakuAST::Name.from-identifier("late"));
is $anon.name.canonicalize, 'late', 'a routine without a name gets one';

# The renamed method still lowers and runs.
my $sub = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier("orig"),
    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(42)))),
);
$sub.replace-name(RakuAST::Name.from-identifier("renamed"));
my $unit = RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => $sub),
    RakuAST::Statement::Expression.new(expression => RakuAST::Call::Name.new(
        name => RakuAST::Name.from-identifier("renamed"))),
);
is EVAL($unit), 42, 'the renamed sub is callable by its new name';

throws-like { $m.replace-name('x') }, Exception, 'replace-name needs a Name';
throws-like { $m.replace-scope(5) }, Exception, 'replace-scope needs a Str';
