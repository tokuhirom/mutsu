use Test;

# Infix applications, named calls and initializers carry the origins Rakudo
# gives them (ADR-10723 Stage 2): an `ApplyInfix` and its `ArgList` span the
# operands, its operator node covers the operator as written, a call's `Name`
# and `ArgList` cover the name and the arguments, and an `Initializer` runs
# from its `=` / `:=` to the end of the expression. Ranges measured on
# rakudo 2026.09.

plan 29;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
my ($x,$a,$b,$c); sub foo(*@) {}
say 1 + 2 * $x;
my $y = $a ~ "b" ~ $c;
foo(1, $x);
my $z := $x;
$x += 1;
$a R- 3;
$x = 1 < 2;
say 1 < 2 < 3;
END
my @s = $src.AST.statements;

my $say = @s[2].expression;
is range(parts($say)[0]), '33..36', "a call's name";
my $say-args = parts($say)[1];
is range($say-args), '37..47', 'the arguments of a call without parentheses';
my $sum = parts($say-args)[0];
is range($sum), '37..47', 'an infix application';
my ($plus, $plus-args) = parts($sum);
is range($plus), '39..40', 'its operator';
is range($plus-args), '37..47', 'its argument list';
my $product = parts($plus-args)[1];
is range($product), '41..47', 'a tighter infix application';
is range(parts($product)[0]), '43..44', 'its operator';

my $init = parts(@s[3].expression)[*-1];
is $init.^name, 'RakuAST::Initializer::Assign', 'an assignment initializer';
is range($init), '55..70', 'it runs from the = to the end';
my $concat = parts($init)[0];
is range($concat), '57..70', 'a left-nested infix chain';
is range(parts($concat)[0]), '66..67', 'its operator';
my $inner = parts(parts($concat)[1])[0];
is range($inner), '57..65', 'the inner application';
is range(parts($inner)[0]), '60..61', 'its operator';

my $call = @s[4].expression;
is range(parts($call)[0]), '72..75', "a call's name";
is range(parts($call)[1]), '76..81', 'the inside of its parentheses';

my $bind = parts(@s[5].expression)[*-1];
is $bind.^name, 'RakuAST::Initializer::Bind', 'a binding initializer';
is range($bind), '90..95', 'it runs from the := to the end';

my $meta = parts(@s[6].expression)[0];
is range($meta), '100..102', 'an assignment metaoperator';
is range(parts($meta)[0]), '100..101', 'the operator it wraps';

my $reverse = parts(@s[7].expression)[0];
is range($reverse), '109..111', 'a reversed operator';
is range(parts($reverse)[0]), '110..111', 'the operator it wraps';

my $assign = @s[8].expression;
is range(parts($assign)[0]), '118..119', 'an assignment operator';
is range(parts(parts($assign)[1])[1]), '120..125', 'the expression assigned';
is range(parts(parts($assign)[1])[0]), '115..117', 'the variable assigned to';
is range(parts(parts(@s[6].expression)[1])[0]), '97..99', 'the variable of a metaop assignment';

my $chain = parts(parts(@s[9].expression)[1])[0];
is range($chain), '131..140', 'a comparison chain';
is range(parts(parts(parts($chain)[1])[0])[0]), '133..134', 'its first link';

my @lines;
sub walk($node) {
    @lines.push: .source.original-line(.from) with $node.origin;
    $node.visit-children(&walk);
}
q:to/END/.AST.visit-children(&walk);
my $t = 1
    +
    2;
END
is @lines.sort.squish.join(','), '1,2,3', 'an operator on its own line is reported';

is EVAL(q[my $q = 2; my $r := $q; $r * 3 + 1].AST), 7, 'the round trip still runs';
