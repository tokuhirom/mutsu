# Reads of a shared Array or Hash run on a snapshot by default

Once a program has started a second thread, a read of an Array or Hash that
another thread is restructuring used to be safe only if its code path had been
found and guarded one at a time. The array side of #13079 widened the
reader-family harness to 44 families, and 22 of them still crashed on a debug
build: slices, `@x[*-1]`, `:exists`, `.List`, `.Array`, `.sort`, `.head`,
`.tail`, `.kv`, `.pairs`, `.first`, `.max`, `.Set`, `(elem)`, `my @c = @x`,
`.gist`, `eqv`, `[+]`, hyper operators and `Z`.

The fix inverts the default instead of adding fifteen more guards (ADR-0068
§20). A new `value::container_read` module holds the policy that every method
entry asks: mutators, leaf reads, identity answers (`.list`, `.Hash`, ...) and
a few receiver-writing callbacks run on the live container, and every other
method runs on a copy taken under the container's lock. Two method entries
that bypassed the old check now apply it too. The readers that are not method
calls are guarded where they read: lazy `.kv`/`.pairs`/`.values` views take the
lock on each pull, `value_to_list` copies under it, and `eqv`, the set
operators, `Z`/`X`, hyper operands, list-assignment sources, `:exists` and the
general subscript path read a shared operand under the lock or as a copy.
`.sort` now reads each element once before sorting, because an element cell
written by another thread mid-sort made Rust's `sort_by` panic.

Single-threaded programs pay one relaxed atomic load at each of these points.
`t/concurrency/concurrent-param-array-reader-families.t` now covers all 44
families.
