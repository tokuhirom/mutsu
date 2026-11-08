use Test;
use experimental :rakuast;

plan 5;

# `visit-children` of a variable declaration reaches the initializer before the
# `shape`, though `.raku` lists the shape first (#15780).
sub visit-order(Str $code) {
    my @seen;
    sub walk($n) { @seen.push($n.^name.split('::').tail); $n.visit-children(&walk) }
    walk($code.AST.statements[0].expression);
    @seen.join(' ');
}

is visit-order('my @a[3] = 1,2,3'),
   'Simple Assign ApplyListInfix Infix IntLiteral IntLiteral IntLiteral SemiList Expression IntLiteral',
   'initializer, then shape';
is visit-order('my Int %h{Str} = 1'),
   'Simple Simple Name Assign IntLiteral SemiList Expression Simple Name',
   'a keyed hash: type, initializer, shape';
is visit-order('my @a[3]'), 'Simple SemiList Expression IntLiteral', 'no initializer: shape alone';
is visit-order('my Int %h{Str} is default(1)'),
   'Simple Simple Name SemiList Expression Simple Name Is Name Parentheses SemiList Expression IntLiteral',
   'without an initializer the traits follow the shape';
is 'my @a[3] = 1,2,3'.AST.statements[0].expression.raku.lines[1].trim, 'shape       => RakuAST::SemiList.new(',
   '.raku still lists the shape first';
