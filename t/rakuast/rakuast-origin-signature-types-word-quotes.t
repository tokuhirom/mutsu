use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a pointy or regex
# signature with non-simple types (`Int:D`, `Array[Int]`) and named
# parameters (`:$y`); an interpolating word quote's segments (`«a $x b»`); a
# `:k«a b»` colonpair value; and a method call on an angle-quoted number
# (`<1/2>.Rat`), which starts at the number inside the brackets.

plan 5;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }

my $b = Q[my $f = -> Int:D $x, Array[Int] :$y { };].AST.statements[0].expression.initializer.expression;
is $b.signature.parameters.map({ o($_) ~ '/' ~ o(.target) ~ '/' ~ o(.type) }).join(' '),
    '11-19/17-19/11-16 21-35/33-35/21-31', 'pointy parameters with non-simple types, a named one';
my $g = Q[grammar G { token word(:$e) { a } }].AST.statements[0].expression.body.body.statement-list.statements[0].expression;
is o($g.signature) ~ ' ' ~ o($g.signature.parameters[0]), '23-26 23-26', 'a named token parameter';
is Q[my $x; say «a $x b»;].AST.statements[1].expression.args.args[0].segments.map(&o).join(' '),
    '12-14 14-16 16-18', 'the segments of an interpolating word quote';
is o(Q[say (:k«a b»);].AST.statements[0].expression.args.args[0].semilist.statements[0].expression.value),
    '7-12', 'a :k«...» value';
my $p = Q[say <1/2>.Rat;].AST.statements[0].expression.args.args[0];
is o($p) ~ ' ' ~ o($p.postfix), '5-13 10-13', 'a method call on an angle-quoted number';
