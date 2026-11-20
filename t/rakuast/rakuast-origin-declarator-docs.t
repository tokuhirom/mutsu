use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the `Doc::Declarator` of a
# documented declaration runs from its first `#|` / `#=` comment to the end of
# its last one, and the comments are trivia to the statement lists around
# them.

plan 6;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Doc|StatementList/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[#| doc for a
sub a { }]),
    'StatementList@13-22 Doc::Declarator@0-12 StatementList@20-20',
    'a leading comment';
is placed(Q[#| one
#| two
sub b { } #= after]),
    'StatementList@14-23 Doc::Declarator@0-32 StatementList@21-21',
    'leading and trailing comments';
is placed(Q[#|( bracketed
doc ) sub c { }]),
    'StatementList@20-29 Doc::Declarator@0-19 StatementList@27-27',
    'a bracketed comment';
is placed(Q[sub d(
  #| p
  $x, #= after
) { }]),
    'StatementList@0-34 Doc::Declarator@9-28 StatementList@32-32',
    'a parameter';
is placed(Q[class C {
  #| attr
  has $.x;
}]),
    'StatementList@0-32 StatementList@22-30 Doc::Declarator@12-19',
    'an attribute';
is placed(Q[sub f {
  1;
  #= trailing inside
}]),
    'StatementList@0-35 Doc::Declarator@15-33 StatementList@10-12',
    'a trailing comment inside a body';
