# The write-through call names have one definition

The names the front ends lower a write through a method call, a named rw
routine or a callable to (`__mutsu_assign_method_lvalue` and five more) were
spelled as string literals in 22 files of the compiler, VM, runtime, TRIR and
the RakuAST lowering. They are now constants in `src/lvalue_call.rs`
(ADR-17198, family 2 of #15761), every site uses them, and `check-frontend-debt`
fails on a quoted spelling outside that module. No behaviour change.
