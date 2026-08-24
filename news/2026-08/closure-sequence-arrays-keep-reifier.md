# Closure-sequence arrays keep their reifier

Endpoint-less closure sequences assigned to an `@` array now retain their live
generator instead of becoming a finite snapshot of the 32-step eager prefix.
Indexing extends the array on demand, display keeps the lazy `[...]` placeholder,
and bounded element mutation preserves both the live tail and the generator's
pristine recurrence history.

The change uses the existing shared `Gc<LazyList>` representation and the
bounded lazy-array mutation restore path. The upstream partially-reified Array
clone tests confirm that clones share generated values while subsequent element
mutations remain independent.
