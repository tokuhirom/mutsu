# `bless` skips the store check for an untyped attribute

Every attribute a `bless` stored, including through a user `new`'s `|%_`
passthrough, went through `check_attr_store_type`. That function walks the
class's MRO by name for the attribute's type constraint and then for a `where`
clause. For an untyped attribute with no `where` clause, such as
`has $.name`, the check finds neither and returns the value unchanged, so the
walk was spent for nothing on every construction.

The ctor plan now records, per attribute, whether either constraint exists.
It is computed on the first store and kept for as long as the class shape
lasts, like the plan's other per-attribute memos. `bless` skips the check
when there is nothing to check.

Measured with callgrind (`MUTSU_JIT=off`) against `main` at `fd420de1`:
`bench-ctor` goes from 1,235.3M to 1,222.9M instructions (-1.0%). A
three-attribute `new(*%_) { self.bless(|%_) }` loop goes from 397.2M to
388.5M (-2.2%).
