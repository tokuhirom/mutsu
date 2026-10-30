use Test;

# Shell::DSL declares `sub infix:«|>»(*@operands) is assoc<list>` and
# `sub infix:«|>>»(...) is assoc<non> is tighter(&infix:«|>»)`. Three gaps kept
# `a |> b |>> c` from reading as `a |> (b |>> c)`: the RakuAST round trip refused
# the `&infix:«|>»` reference (the symbol holds a `>`), the reference to a
# declared operator with no precedence trait of its own had no level, and a
# module's scan lost that level once its parse scopes were gone.

plan 6;

sub infix:«|>»(*@o) is assoc<list> { "pipe(" ~ @o.join(",") ~ ")" }
sub infix:«|>>»($a, $b) is assoc<non> is tighter(&infix:«|>») { "app($a,$b)" }

is (1 |> 2 |>> 3), 'pipe(1,app(2,3))', 'the tighter operator binds first (same file)';
is (1 |>> 2 |> 3), 'pipe(app(1,2),3)', 'and on the other side';
is (1 |> 2 |> 3 |>> 4), 'pipe(1,2,app(3,4))', 'the list-associative pipe still collects its operands';

{
    use lib 't/lib';
    use PrecTraitGuillemet;
    is (1 |> 2 |>> 3), 'pipe(1,app(2,3))', 'the imported tighter operator binds first';
    is (1 |>> 2 |> 3), 'pipe(app(1,2),3)', 'and on the other side';
    is (1 |> 2 |> 3 |>> 4), 'pipe(1,2,app(3,4))', 'the imported pipe collects its operands';
}
