# RakuAST: `my %h is SetHash` and `my $x is dynamic`

A re-survey after the method-call assignment slice put "declaration with
traits" at the top of the `.AST` refusals: 82 `t/` files. Most were container
types on a variable: `is SetHash`, `is BagHash`, `is Map`, `is List`, or a
class the file declares.

Measured on rakudo 2026.09, `is NAME` renders as
`Trait::Is(type => Type::Simple(NAME))` when `NAME` resolves to a type at
parse time, and as `Trait::Is(name => NAME)` otherwise. The parser records
the trait as a bare `(NAME, None)` entry. The converter now applies the same
test through the bareword resolver: a builtin type or one the unit declares.
Lowering turns the `type` form back into that entry.

Writing the test showed that `my $x is dynamic` rendered wrongly. The parser
keeps it as the declaration's `is_dynamic` flag, which the converter rendered
as a `*` twigil, as if the source had said `my $*x`. It now renders rakudo's
`Trait::Is(name => dynamic)` and keeps the plain name. The parser does not
record where `is dynamic` stands among the other traits, so beside another
trait it now declines rather than guess the order.

That same test also found an unrelated frontend bug: a missing `$*x` read in
a named sub yields Nil instead of a Failure (#11728).
