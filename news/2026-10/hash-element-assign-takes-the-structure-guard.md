# Concurrent `%!h{$k} = v` takes the container-structure guard

The nightly GC-stress TAP run (#12491) panicked in `NanBox::clone` inside
`existing_element_container`: the named-variable element-assign op read and
restructured a hash that another thread was inserting into, an unguarded
use-after-free left over from #11701 (which guarded `++`, `:delete` and leaf
reads but not plain `%!h{$k} = v`). `exec_index_assign_expr_named_op` now takes
the same ADR-0068 structure guard, keyed the same way, and is a no-op until a
second VM mutator thread exists.
