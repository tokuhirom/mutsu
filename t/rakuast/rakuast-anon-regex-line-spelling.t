use Test;

# #18291: `$?LINE` in a code block of an anonymous `token`/`regex`/`rule`
# term keeps its spelling in the RakuAST view (`Var::Compiler::Line`), as it
# does in a `/ ... /` literal, while matching still reads the line it is on.

plan 5;

is Q[my $t = token { a { @seen.push($?LINE) } }].AST.DEPARSE,
    "my \$t = token \{ a \{\n    \@seen.push(\$?LINE)\n\} \}\n",
    'anonymous token';
is Q[my $t = regex { a { $?LINE } }].AST.DEPARSE,
    "my \$t = regex \{ a \{\n    \$?LINE\n\} \}\n",
    'anonymous regex';
is Q[my $t = rule { a { $?LINE } }].AST.DEPARSE,
    "my \$t = rule \{ a \{\n    \$?LINE\n\} \}\n",
    'anonymous rule';

my @seen;
my $t = token {
  a
  { @seen.push($?LINE) }
};
"a" ~~ $t;
is-deeply @seen, [22], 'matching reads the line the code block is written on';

my $ast = Q[my @s; my $q = token {
 c { @s.push($?LINE) } }; "c" ~~ $q; @s].AST;
is-deeply EVAL($ast), [2], 'EVAL of the RakuAST tree reads the written line';
