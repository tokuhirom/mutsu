> **Largely fixed 2026-09-06** —
> `news/2026-09/custom-io-handle-routing-reaches-every-dispatch-entry.md`,
> pinned by `t/custom-io-handle-write-read.t` (6 rows against raku v2026.07).
> Both worked examples from `Type/IO/Handle.rakudoc` now route through the user's
> `WRITE`/`READ`/`EOF`. The remaining scope is narrower than this file's, and it
> is recorded at the bottom under "What is left".
>
> The routing this file says is "completely unimplemented" was in fact already
> implemented (`try_user_io_handle_method`, `vm/vm_call_method_compiled_io.rs`)
> and merely **wired into two of the interpreter's four method-dispatch entry
> points**. The `$*OUT = $store` idiom goes through a third (an internal
> `.print` dispatch from `write_to_named_handle`) and the read-side methods
> through a fourth (the mut path), so both fell through to the native
> `IO::Handle` arm. A third defect sat inside the routing itself: the write
> dispatch's catch-all arm returned early, so a handle overriding BOTH `WRITE`
> and `READ` could never reach its own read methods.

# Custom `IO::Handle` subclasses overriding WRITE/READ/EOF are not honored by print/say/read

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Type/IO/Handle.rakudoc:959`
and `:1013` — the "Creating Custom Handles" section's own worked examples).

## What's broken

`IO::Handle.rakudoc` documents a whole feature (6.d language, "Creating Custom
Handles"): a user class `is IO::Handle` that implements `.WRITE`/`.READ`/`.EOF` gets
all the textual read/write methods (`.print`, `.say`, `.get`, `.lines`, `.read`, ...)
"for free" via the base `IO::Handle` class dispatching to those overridable primitives.
This is completely unimplemented in mutsu: native `.print`/`.say`/`.read` write/read
directly against a real OS file descriptor (or real stdout) and never consult a
subclass's `.WRITE`/`.READ`/`.EOF` overrides.

## Minimal repros (both straight from the doc)

### 1. Redirecting `$*OUT` (or `$PROCESS::OUT`) to a custom WRITE-overriding handle

```raku
class IO::Store is IO::Handle {
    has @.lines = [];
    submethod TWEAK { self.encoding: 'utf8'; }
    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) {
        @!lines.push: data.decode();
        True;
    }
    method gist() { return @!lines.join("\n"); }
}
my $store = IO::Store.new();
my $output = $*OUT;
$*OUT = $store;
.say for <one two three>;
$*OUT = $output;
say $store.lines();
```

- `raku`: `[one\n two\n three\n]` — every `say` was routed through `.WRITE` and
  captured into `@lines`; nothing printed to the real stdout during the redirect.
- `mutsu`: prints `one`/`two`/`three` straight to the real stdout (the redirect is
  ignored), then `$store.lines()` is `[]` (empty — `.WRITE` was never called).

Confirmed this reproduces identically for both `$PROCESS::OUT = $store` (the doc's own
form) and the more common `$*OUT = $store` (tested directly). Root-cause hint: `say`
compiles to `write_to_named_handle("$*OUT", ...)`
(`src/vm/vm_data_io_ops.rs::exec_say_op` → `src/runtime/io_env.rs::write_to_named_handle`),
which *does* attempt `self.call_method_with_values(handle, "print", ...)` for a handle
without a native `handle_id` before falling back to real stdout — so the intent is
there, but that `.print` dispatch onto the `IO::Store` instance is evidently failing
(or `IO::Handle`'s inherited native `.print` doesn't itself call back into the
subclass's `.WRITE`), silently falling through to `emit_output` (real stdout).

### 2. A custom READ/EOF handle used for output *and* input (`.print` + `.read`)

```raku
class IO::Store is IO::Handle {
    has @.lines = [];
    submethod TWEAK { self.encoding: 'utf8'; }
    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) { @!lines.push: data; True; }
    method whole() {
        my Buf $everything = Buf.new();
        for @!lines -> $b { $everything ~= $b; }
        return $everything;
    }
    method READ(IO::Handle:D: Int:D \bytes --> Buf:D) {
        my Buf $everything := self.whole();
        return $everything;
    }
    method EOF { my $everything = self.whole(); !$everything; }
}
my $store := IO::Store.new();
$store.print( $_ ) for <one two three>;
say $store.read(3).decode;   # OUTPUT: «one␤»
say $store.read(3).decode;   # OUTPUT: «two␤»
```

- `raku`: `one` then `two`.
- `mutsu`: dies immediately with `Expected IO::Handle` (a type-check inside the native
  `.print`/`.read` dispatch that rejects a non-native-backed `IO::Handle` subclass
  instance outright).

## Why this is a deep ticket

Fixing this properly means every native IO::Handle read/write entry point
(`.print`/`.put`/`.say`/`.printf`/`.write`, and `.read`/`.readchars`/`.get`/`.getc`/
`.lines`/`.words`/`.slurp`) needs a "does this handle have a real native `handle_id`,
or is it a user subclass with `.WRITE`/`.READ`/`.EOF` overrides?" branch, and the
override branch needs to actually call back into the interpreter's method dispatch
(recursively, since `.WRITE`/`.READ` are themselves regular user methods that can do
anything). That is a systemic change across `src/runtime/native_io/io_handle.rs`,
`src/runtime/handle_open.rs`, and the say/print/note VM ops
(`src/vm/vm_data_io_ops.rs`), not a single-site fix — hence `todo/deep/` rather than
`todo/tickets/`.

## Affected files (starting point)

- `src/runtime/io_env.rs::write_to_named_handle` — the `$*OUT`/`$*ERR` redirect path;
  already has *some* handle-without-`handle_id` fallback logic that should be the
  right shape once `.print` dispatch onto a user `IO::Handle` subclass actually works.
- `src/runtime/native_io/io_handle.rs` — native `.print`/`.read`/etc. dispatch; needs a
  branch that checks for user-defined `.WRITE`/`.READ`/`.EOF` before assuming a native
  backing handle.
- `src/runtime/handle_open.rs` — `IoHandleState`/`IoHandleTarget` may need a new target
  variant for "backed by user WRITE/READ methods, not a real fd".

## What is left (measured 2026-09-06)

Two things, both narrower than the original report and neither about the routing:

1. **A `READ` that over-returns is not buffered.** Raku's `IO::Handle.read($n)`
   keeps what `READ` hands back beyond `$n` and serves the next read from it —
   which is why `Type/IO/Handle.rakudoc`'s second example, whose `READ` ignores
   its byte count and returns the whole buffer every time, prints `one` then
   `two` under raku. mutsu returns whatever `READ` gave, so it prints the whole
   buffer twice. A well-behaved `READ` that honours its count and advances a
   position works correctly today (pinned). Closing this means giving the user
   handle a read buffer; `read_user_io_char` currently assumes `READ(1)` returns
   exactly one byte, so it wants the same buffer.

2. **A separate, pre-existing bug found while testing this, which is NOT about
   custom handles at all**: declaring *any* class with a `print` method makes an
   *unrelated* class's `$*OUT = $handle` redirect fall through to the real
   stdout.

   ```raku
   class Store is IO::Handle { has @.lines = []; submethod TWEAK { self.encoding: 'utf8' }
       method WRITE(IO::Handle:D: Blob:D \d --> Bool:D) { @!lines.push: d.decode; True } }
   class Cap { has $.buf is rw = ""; method print(*@a) { $!buf ~= @a.join; True } }   # <-- just declaring this
   my $store = Store.new; my $old = $*OUT;
   $*OUT = $store; say "one"; $*OUT = $old;
   say $store.lines;    # ["one\n"] without the Cap declaration, [] with it
   ```

   Bisected: the block passes on its own and fails as soon as a class with a
   `print` method exists anywhere in the file. `write_to_named_handle` then takes
   its `handle_id_from_value(...).is_some()` branch (the real-fd path) for the
   custom handle, so the user routing is never consulted. That is the shape of a
   name-keyed "a user overrode this native method" check that is not scoped to
   the receiver's class — `native_lever_a_user_override` is the obvious
   candidate. It deserves its own ticket once someone confirms the site.
