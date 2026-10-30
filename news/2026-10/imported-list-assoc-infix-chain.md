# An imported `is assoc<list>` infix takes a whole operand chain

`1 op 2 op 3` with an imported plain `sub infix:<op> is assoc<list>` died with
"Two terms in a row": a block's lexical import binds a plain routine in its
import set, and only the two-operand `InfixFunc` site consulted that set. The
compiler now emits the `LexImportInfix` prefix for a chain too, and the VM calls
the bound routine with all operands (a `chain`-associative operator and a multi
family dispatcher keep the generic multi-operand path). Found with
FunctionalParsers `t/12-alternatives-first-match.rakutest` (#15537).
