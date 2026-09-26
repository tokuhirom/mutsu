# Method calls stop paying for attributes, locals and frames nobody reads

Refs [#9494](https://github.com/tokuhirom/mutsu/issues/9494).

`use Services::PortMapping` parses a 15,000-line CSV file with Text::CSV
while the module loads, and under mutsu that took about 125 seconds, over
the ecosystem sweep's 120-second budget. The load was never hung. Every
parsed field went through a few dozen method calls, and each call did work
that grew with things unrelated to the call itself.

- **Attribute count.** On the way out of every method, `reconcile_attrs`
  built seven twigil spellings (`!x`, `.x`, `@!x`, ...) of *every*
  attribute of the receiver with `format!` and looked each one up. Text::CSV
  has about 45 attributes. The scan now starts from the frame's
  `ContainerRef` values (usually none) and matches them against the
  attribute keys. The per-call `is default(...)` registration asked two
  `(String, String)`-keyed tables about every attribute. Those tables are
  now keyed class-first (`ClassAttrTable`), so a class with no defaults is
  one probe. Where defaults do exist, the six variable names come from a
  memo and an unchanged entry is left alone.
- **Caller's locals.** After every call, `apply_pending_caller_var_writeback`
  hashed each of the caller's locals against a pending set that usually
  holds two or three names. It now searches from whichever side is smaller.
- **Caller's scope.** Once a method calls another method, its scoped
  environment is flattened into a copy of the whole visible scope. The
  return merge then compared every entry of that copy. It now walks the
  frame-write log the flatten leaves behind, which the light sub-call merge
  has done since #7630.
- **Dispatch frames nobody reads.** A multi-method call built a deferral
  frame for `callsame` & co. by expanding, cloning and argument-matching
  every candidate. It skips this when the program never names a deferral
  builtin. The existing global latch is now set from each compilation
  unit's source text and constant pool, and it also covers `nextcallee`
  and `lastcall`.
- **Resolution caches that never hit.** The type-keyed multi-resolution
  cache refused a `ContainerRef` argument (a variable a closure captured),
  and it treated every `::`-qualified type constraint as a value
  refinement. So `CSV::Row.push(CSV::Field $f)` re-ran the full candidate
  walk for each field. A captured argument now keys by its contents, and a
  qualified name that declares a class or role counts as nominal.
- **Constructors.** `CSV::Field.new` has one user candidate,
  `new(Str(Cool) $str)`. With no arguments it fell back to `Mu.new` by
  formatting an `X::Multi::NoMatch` message and throwing it away, then ran
  the slow default constructor. When every user `new` candidate declines the
  arguments, the native default constructor now builds the object directly.
- **Type checks.** `is_my_scoped_type_name` scanned every registered type
  key, building a `format!` prefix, on each type check of a qualified class
  name. It now probes the exact key and scans only for names that have
  lexically scoped registrations.

Parsing 100 rows of the IANA port list went from 38.7M to 19.6M
instructions per row (callgrind, warm cache). Locally, loading the module
went from over 120 seconds to about 60. The issue closes when the
ecosystem sweep's own ledger record stops reporting `SWEEP-TIMEOUT`.
