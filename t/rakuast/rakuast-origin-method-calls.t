use Test;

# Each method call of a chain carries the origins Rakudo gives it
# (ADR-10723 Stage 2): the partial chain it applies to, the `Call::Method`
# from the method name to the end of the call, its `Name`, and the inside of
# its parentheses. Ranges measured on rakudo 2026.09.

plan 12;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
say @a.map({ $_ }).sum;
say @a.map(*.abs).sum;
END
my @a; my @s = $src.AST.statements;

my $chain = @s[0].expression.args.args[0];
is range($chain), '4..22', 'a whole chain';
my ($partial, $sum) = parts($chain);
is range($partial), '4..18', 'the partial chain the last call applies to';
is range($sum), '19..22', 'the last call, from its name';
is range(parts($sum)[0]), '19..22', "the last call's name";
my $map = parts($partial)[1];
is range($map), '7..18', 'a call with arguments';
my ($name, $args) = parts($map);
is range($name), '7..10', 'its name';
is range($args), '11..17', 'the inside of its parentheses';

my $map-args = parts(parts(parts(@s[1].expression.args.args[0])[0])[1])[1];
my $curried = parts($map-args)[0];
is range($curried), '35..40', 'a curried `*.abs` argument';
is range(parts($curried)[0]), 'none', 'its `*` has no origin';
is range(parts($curried)[1]), '37..40', 'the method it calls on `*`';
is range(parts(parts($curried)[1])[0]), '37..40', 'its name';

is EVAL(q[my @a = 1, -2; @a.map(*.abs).sum].AST), 3, 'the round trip still runs';
