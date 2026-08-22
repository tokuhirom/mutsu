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
