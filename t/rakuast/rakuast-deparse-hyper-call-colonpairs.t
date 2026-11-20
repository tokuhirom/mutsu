use Test;
use experimental :rakuast;

plan 21;

# `@a».(args)` calls each element: `MetaPostfix::Hyper(Call::Term)` (#14845;
# measured on rakudo 2026.11).
my @a = { $_ * 2 }, { $_ + 1 };
is Q[@a».(3)].AST.DEPARSE.trim, '@a>>(3)', '@a».(3)';
is Q[@a».(3)].AST.statements[0].expression.postfix.postfix.^name, 'RakuAST::Call::Term', 'over a Call::Term';
is-deeply @a».(3), [6, 4], 'the hyper call still calls';
is-deeply EVAL(Q[my @b = { $_ * 2 }; @b».(4)].AST), [8], 'EVAL of a hyper call';

# Colonpairs one after another as a term are `ColonPairs`.
is Q[my @x = :a:!b:42c].AST.DEPARSE.trim, 'my @x = :a:!b:42c', ':a:!b:42c';
is Q[my $c = :a :b].AST.DEPARSE.trim, 'my $c = :a:b', ':a :b';
is Q[my $c = :a:b].AST.statements[0].expression.initializer.expression.^name, 'RakuAST::ColonPairs', 'as ColonPairs';
is Q[my $c = :a:b].AST.statements[0].expression.initializer.expression.colonpairs.elems, 2, 'with its colonpairs';
is Q[:a:!b].AST.DEPARSE.trim, ':a, :!b', 'a statement of colonpairs stays a list';
is-deeply EVAL(Q[my $c = :a:!b:42c; $c].AST), $(:a, :!b, :c(42)), 'EVAL of ColonPairs';
is RakuAST::ColonPairs.new(RakuAST::ColonPair::True.new("x")).raku,
    qq:to/END/.chomp, '.raku of a constructed ColonPairs';
    RakuAST::ColonPairs.new(
      RakuAST::ColonPair::True.new("x")
    )
    END

# `subset ::` keeps its anonymous name, `Name(EmptyEdge, EmptyEdge)`.
is Q[my $s = (subset :: where Int | Str)].AST.DEPARSE.trim, 'my $s = (subset :: where Int | Str)', 'anonymous subset';
is Q[(subset :: of Real where * > 3)].AST.DEPARSE.trim, '(subset :: of Real where * > 3)', 'with of';
ok 42 ~~ (subset :: where Int | Str), 'the anonymous subset still matches';
ok EVAL(Q[my $t = (subset :: where Int); 5 ~~ $t].AST), 'EVAL of an anonymous subset';

# A declarator comment keeps its text as written in RakuAST; `.WHY` answers
# each line trimmed and joined by a space.
my $docs = q:to/END/.AST;
class Simple {
#={
simple case
}
}
sub third { #={     leading space here}
}
sub f { #=   foo   bar
}
END
is $docs.DEPARSE.lines[1], '#= ', 'a bracketed comment keeps its first line break';
is $docs.DEPARSE.lines[4], 'sub third { #=      leading space here', 'and its leading space';
is $docs.DEPARSE.lines[6], 'sub f { #=   foo   bar', 'a line comment keeps its spacing';
is-deeply $docs.statements[0].expression.WHY.trailing, ("\nsimple case\n",), 'the raw trailing text';
sub g { #={  a
  b   c }
}
is &g.WHY.trailing, 'a b   c', '.WHY trims each line and joins them';
sub h { #=   x   y
}
is &h.WHY.trailing, 'x   y', 'a line comment is trimmed';
