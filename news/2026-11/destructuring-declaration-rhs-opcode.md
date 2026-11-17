# `my ($a, $b) = f()` no longer calls a helper by name or builds a list it throws away

A list-assigning declaration is lowered to a small block: the targets are
declared, a staging array is assigned from
`__mutsu_list_assign_rhs(RHS)`, each target reads its element, and the block
yields the list of the targets. Two parts of that did no useful work in the
common case:

- the helper, which only removes one level of itemization from the right-hand
  side, was called through the general by-name function dispatch, about 11k
  instructions per declaration;
- in statement position the block's value, `($a, $b)`, was built and then sunk,
  which observes nothing.

A right-hand side that is a call, a method call or an array variable now
compiles to a `ListAssignRhs` opcode, and the sunk form of the declaration no
longer builds its result list. A routine whose last statement is the
declaration, or an expression that uses its value, still gets the list.

The helper and the opcode share one implementation, which now also removes the
Scalar container an `is rw` routine returns. `my ($a, $b) = f()` where `f`
returns a variable holding `[5, 6]` assigns 5 and 6, as in Rakudo; it used to
put the whole array in `$a`.

MCP::Server::Tool::Web's tokenizer runs `my ($name, $after-name) =
read-name(...)` at every tag.
