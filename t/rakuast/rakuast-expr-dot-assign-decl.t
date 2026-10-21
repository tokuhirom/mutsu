use Test;
use experimental :rakuast;

# A `.=` declaration inside an expression keeps its source form
# (`Initializer::CallAssign`) in the AST while the declaration still registers
# its lexical and stays a writable lvalue. Checked against `raku` first.

plan 7;

my $typed = Q|(my Int $x .= new)|.AST.gist;
ok $typed.contains('Initializer::CallAssign'),
    'a parenthesized typed declaration keeps the CallAssign initializer';
nok $typed.contains('ApplyPostfix'), 'no expanded call is rendered';
ok Q|(my $y .= new)|.AST.gist.contains('Initializer::CallAssign'),
    'an untyped declaration keeps it too';

is (my Int $a .= new), 0, 'the expression-position declaration still runs';
is $a, 0, 'and registers its lexical';
(my Int $b .= new).=new(42);
is $b, 42, 'the declaration stays a writable lvalue';
is EVAL(Q|(my Int $c .= new).=new(42); $c|), 42, 'EVAL of the source agrees';
