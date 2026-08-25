# `Proc::Async.new` no longer turns an unrecognised named argument into an argv element

`Proc::Async.new(:r, 'echo', 'Raku')` died with

```
Failed to spawn 'r\tTrue': No such file or directory (os error 2)
```

instead of running `echo Raku`. The constructor
(`build_native_proc_async_value`, `src/runtime/methods_object_native_ctors_io.rs`)
recognised exactly three named pairs — `:w`, `:out`, `:enc` — and its catch-all
arm pushed everything else into `positional`, which becomes the `cmd` attribute
`.start()` consumes. An unrecognised named `Pair` therefore ended up *ahead of
the program name* in argv, stringified as `"r\tTrue"`, and `Command::new()` tried
to spawn a program by that name.

## What Rakudo actually does

Measured against rakudo 2026.06 rather than assumed. `Proc::Async.^lookup('new').signature`
is `(Proc::Async $:: |)`, and the real candidate reported by a failed dispatch is

```
(Proc::Async $:: *@args where { ... }, *%_)
```

Every named argument — the ones `Proc::Async` uses (`:w`, `:enc`,
`:translate-nl`, `:arg0`, `:win-verbatim-args`, `:pty`, `:started`) and every one
it does not (`:r`, `:in`, a typo) — lands in the slurpy `*%_` and is silently
absorbed. Only genuine positionals become the command, and the `where .so`
constraint makes a command-less call an `X::Multi::NoMatch`, which mutsu already
matched. (The `:path`/`:args` multi shown in `raku-doc` does **not** exist in
this Rakudo — `Proc::Async.new(:path<echo>, :args["hi"])` fails to resolve — so
it was deliberately not implemented.)

A *positionally* written pair is different and stays in argv:
`Proc::Async.new("echo", ("bogus" => 1)).args` really is `["echo", :bogus(1)]`.

## The fix

The constructor's catch-all now discriminates on pair flavour rather than on the
key name: a `ValueView::Pair` is the named-argument flavour (ADR-0021's
`OpCode::MakeNamedArg`) and is absorbed unconditionally; a `ValuePair` is a
data-minted pair written as a real positional and is still pushed into `cmd`.
That is a general rule, not a list of blessed key names, so `:translate-nl`,
`:win-verbatim-args` and any future named cost nothing to accept.

Pinned by `t/proc-async-divergences.t`.
