use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

# `RakuAST::Node.literalize` (#14916): the value of a constant node, or the
# `RakuAST::Node::CannotLiteralize` type object (answered, not thrown).

sub lit(Str $source) {
    $source.AST.statements[0].expression.literalize
}

my @constants =
    '1 + 2'        => 3,
    '2 * 3 + 4'    => 10,
    '(1 + 2) * 3'  => 9,
    '"a" ~ "b"'    => 'ab',
    '"a" x 2'      => 'aa',
    '"5" + 1'      => 6,
    '-5'           => -5,
    '-"3"'         => -3,
    '1.5'          => 1.5,
    '1/2'          => 0.5,
    '2 ** 100'     => 1267650600228229401496703205376,
    '2 ** -1'      => 0.5,
    '5 % 3'        => 2,
    '5 div 2'      => 2,
    '10 mod 3'     => 1,
    '12 gcd 8'     => 4,
    '1 min 2'      => 1,
    '3 +& 1'       => 1,
    '1 +^ 2'       => 3,
    '1 < 2'        => True,
    '"a" lt "b"'   => True,
    '1 ~~ Int'     => True,
    '1 === 1'      => True,
    '!True'        => False,
    'so 1'         => True,
    'not 1'        => False,
    '1 && 0'       => 0,
    '1 // 2'       => 1,
    '1 or 2'       => 1,
    'True && "x"'  => 'x',
    'Inf'          => Inf,
    'pi'           => pi,
    'e'            => e,
    'tau'          => tau,
    'Int'          => Int,
    'Bool::True'   => True,
    '1e3'          => 1e3,
    "'a'"          => 'a',
    'qq{a}'        => 'a',
    '+(1,2)'       => 2,
;
for @constants -> (:key($source), :value($expected)) {
    is-deeply lit($source), $expected, "$source literalizes";
}

is-deeply lit('1..3'), 1..3, 'a range';
is-deeply lit('5 <=> 3'), More, 'an Order';
is-deeply lit('a => 1'), (a => 1), 'a fat arrow';
is-deeply lit(':a(1)'), (:a(1)), 'a colonpair with a value';
is-deeply lit(':!a'), (:!a), 'a false colonpair';
is-deeply lit('(1, "a")'), (1, 'a'), 'a parenthesised list';
is-deeply lit('1,2'), (1, 2), 'a comma list';
is-deeply lit('()'), (), 'the empty list';
is-deeply lit('<a b c>'), ('a', 'b', 'c'), 'a words quote';
is-deeply lit('[1, 2, 3]'), (1, 2, 3), 'an array composer answers a List';
is lit('0 but True').Bool, True, 'but';
is lit('0 but True') + 1, 1, 'but keeps the value';

for 'say 1', 'my $x', '1 + rand', '(1,2)[0]', '1 ?? 2 !! 3', '"abc".chars', 'Int:D', '$_',
    'abs(-1)', '-1.abs', '*', '1 .. *', '[+] 1,2', 'Int.new', '"a".uc', '1 ff 2' -> $source {
    my $got = lit($source);
    ok !$got.defined && $got ~~ RakuAST::Node::CannotLiteralize, "$source cannot be literalized";
}

# A hand-built node, and the statement and list wrappers.
is RakuAST::ApplyInfix.new(left => RakuAST::IntLiteral.new(2), infix => RakuAST::Infix.new('*'),
    right => RakuAST::IntLiteral.new(21)).literalize, 42, 'a hand-built node';
is RakuAST::StrLiteral.new('a').literalize, 'a', 'a string literal';
is RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(7)).literalize, 7,
    'a statement';

# Nothing stateful runs: the operand of `++` is not evaluated.
my $x = 5;
ok !lit('$x++').defined, 'a postfix increment is not a constant';
is $x, 5, 'and did not run';

done-testing;
