use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 5;

our $pk = "q";
package P { our $v = "q" }

ok EVAL(Q[/ $P::v /].AST) ~~ Regex, 'a tree with a package-qualified regex interpolation evaluates';
ok "q" ~~ EVAL(Q[/ $OUR::pk /].AST), '$OUR::pk';
ok "q" ~~ EVAL(Q[/ $P::v /].AST), '$Pkg::v';
nok "r" ~~ EVAL(Q[/ $P::v /].AST), '$Pkg::v does not match another string';
ok "qq" ~~ EVAL(Q[/ ^ $P::v+ $ /].AST), 'quantified';
