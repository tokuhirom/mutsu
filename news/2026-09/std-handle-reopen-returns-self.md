# `$*OUT.open` reopens the standard stream instead of creating a file named `STDOUT`

`$*OUT.open: :w, :!out-buffer` — the way `Testo::Out::TAP` builds its output
handles — used to create a **real file called `STDOUT` in the current
directory** and silently redirect the process's own output into it. Every
symptom was invisible: the program printed nothing, the exit status explained
nothing, and the working directory grew a pair of files named `STDOUT` and
`STDERR`. A `Testo`-based test suite emitted no TAP at all, not even its plan
line, so the ecosystem sweep recorded `plan: null` with nothing to go on
(`ecosystem/dists/P/Proc--Q~c1053504.json`, surfaced once
[#8403](https://github.com/tokuhirom/mutsu/issues/8403) stopped `RakudoPrereq`
from blocking `Proc::Q`'s load).

## Root cause

A standard handle's `path` *attribute* is the sentinel name `"STDOUT"`; only the
`.path` *method* turns it into `IO::Special.new("<STDOUT>")`. `IO::Handle.open`
read the attribute, handed it to `resolve_path`, and opened a file of that name.
Because `.open` writes the opened handle back over its receiver
(`native_io_handle_mut`, so `$fh.open; $fh.print` works), `$*OUT` itself then
pointed at the file — which is why the *subsequent* plain `say` landed there
too.

The `open` **sub** already had the right handling: `builtin_open` recognises an
`IO::Special` argument and hands back a fresh handle on the live stream. Only
the method form was missing it.

## Fix

`IO::Handle.open` now checks the receiver's entry in the handle table first. A
handle whose target is not a file has nothing to open, so mutsu applies the
per-handle options the caller passed and returns the receiver itself — matching
Rakudo, where `$*OUT.open(:w) === $*OUT` is `True`.

Only *explicitly passed* options are applied. Rakudo defaults each of `:chomp`,
`:nl-in`, `:nl-out`, `:out-buffer`, `:bin` and `:enc` to the handle's current
value rather than to a fresh default, so an earlier `$*OUT.nl-out = "|"`
survives a later `$*OUT.open(:w)`; this was measured against `raku` rather than
assumed.

Making `$h === $*OUT` answer `True` needed a second change. An `IO::Handle`
value is a thin wrapper over a handle-table id, and mutsu re-wraps the same id
into a fresh instance whenever a handle is handed back, so instance-identity
`===` said `False`. `values_identical` now compares the handle id for two open
`IO::Handle`s; an unopened `IO::Handle.new(:path($p))` carries no id and keeps
plain instance identity, so two of those are still not `===` (raku agrees).

The `open` sub keeps its fresh-handle behaviour: `open($*OUT.path, :w) === $*OUT`
is `False` in Rakudo too.

Pinned by `t/io/std-handle-reopen-returns-self.t`, which also runs clean under
`raku`. Closes [#8428](https://github.com/tokuhirom/mutsu/issues/8428).
