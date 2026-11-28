# JSON::Fast's `str-escape` runs in TRIR

Every string `to-json` writes, keys included, goes through JSON::Fast's `str-escape(\text)`: a
per-codepoint `nqp::while` over `text.NFD`. TRIR (ADR-0110) declined the routine on three
constructs, so each character paid the untyped bytecode path, about 5.7k instructions for
three generic `nqp::` dispatches. An 8,000-character string took 8.2 ms to encode against
rakudo's 0.12 ms.

The three constructs, now admitted:

- `.NFD`, `.NFC`, `.NFKD`, `.NFKC`, `.chr` and `.ord` join TRIR's value-only methods, and the
  receiver of such a method may be a sigilless parameter, which it reads for its value exactly
  as an `nqp::` operand does.
- Postfix `$i++` / `$i--` on a native int slot: the old value, then the sink form of the prefix
  step.
- A sigilless parameter handed a variable (`str-escape($_)`). It binds the caller's container,
  which a TRIR slot holding a value cannot stand in for, so every door declined such an
  argument. The compiler now proves when the two are indistinguishable: every read of the
  parameter runs before the body's first op that could run foreign code or write anything but
  the frame's own slots, and no jump can bring control back above a read. Then the value at
  entry is the value every read sees, and the doors bind it (`TrParam::value_at_entry`). A read
  after a call, or inside a loop, still declines, so a callee that changes the bound variable
  stays visible (`t/lang/trir-sigilless-value-at-entry.t` pins both).

On `bench-json-fast` (release, local 4-core container, warm cache): the warm section went from
~56 ms to ~16.6 ms (median of 7), whole-process instructions from 760.9M to 329.2M and
allocations from 134,474 to 106,589 (`scripts/bench-det.sh`, `+jit` lane). The long-string
encode alone went from 8.2 ms to 0.42 ms.
