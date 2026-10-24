# A set operator's compound assignment parses in expression position

`$s (|)= 2`, `$s ∪= 3` and `$s (&)= 2` used to work only as statements (#14967).
In a call argument (`say($s (|)= 2)`), a listop argument (`say $s (|)= 2`), an
initializer (`my $r = $s ∪= 3`) or parentheses, mutsu reported a parse
error, or `X::Syntax::Malformed` for an initializer.

The expression parser (`logic.rs`), the call-argument parser and the listop
argument parser now also read a set operator's compound assignment. Each
builds the same typed compound-assignment node as the statement form (#14948),
so `.AST` keeps the set operator there too.

`t/collections/set-bag-mix/set-compound-assign-expression.t` covers the
positions.
