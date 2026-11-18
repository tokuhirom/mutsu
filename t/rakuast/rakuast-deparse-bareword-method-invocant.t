use Test;
use experimental :rakuast;

plan 4;

# `make-seq.tail`: a paren-less call of a declared sub as a method invocant is
# `ApplyPostfix(Call::Name::WithoutParentheses, Call::Method)` (#14845).
my $src = 'sub make-seq { 1 }; my $x = make-seq.tail;';
my $ast = $src.AST;
is $ast.find-nodes(RakuAST::Call::Name::WithoutParentheses).elems, 1, 'the call is written without parentheses';
is $ast.find-nodes(RakuAST::Call::Name::WithoutParentheses)[0].name.canonicalize, 'make-seq', 'its name';
is $ast.DEPARSE.lines.tail.trim, 'my $x = (make-seq).tail', 'DEPARSE parenthesises the invocant call';
is EVAL('sub make-seq { <a b c> }; make-seq.tail'.AST), 'c', 'EVAL of the AST still calls it';
