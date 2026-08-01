# Cro HTTP request round-trip hangs: cross-module short-name env pollution breaks `Header.parse`

## Where the Cro campaign stands

`use Cro::HTTP::Router`, `route { get -> { ... } }`, `use Cro::HTTP::Server`,
`Cro::HTTP::Server.new(:host, :port, :application($app)).start` / `.stop` all
work (#5658, #5666, #5668). Serving an actual HTTP request does NOT: the
connection is accepted, the request bytes reach `Cro::HTTP::RequestParser`, the
request line and the first header line parse — and then the request dies inside
the parser, so no response is ever written and the client hangs.

Repro (environment prepared under `tmp/cro-work/`, see
`handoff-cro-http-router-load` memory):

```
timeout 90 target/debug/mutsu $(cat tmp/cro-work/inc-paths.txt) tmp/cro-server-request.p6
```

(zsh note: expand `inc-paths.txt` inline with `$(cat ...)`; a `$VAR` expansion
is not word-split and collapses every `-I` into one argument.)

## Root cause chain (fully diagnosed, deterministic, main thread)

1. `Cro::HTTP::Header` (Header.rakumod) contains a class-body lexical grammar:
   `class Cro::HTTP::Header { my grammar Header { ... } ... method parse(...) }`.
   `method parse` calls bare `Header.parse($value)` — the lexical grammar.
2. `Cro::HTTP::Router::Roles` declares `package Cro::HTTP::Router { role Header {} ... }`.
   Registering that role does two things to the GLOBAL env/state:
   - `exec_register_role_op` calls `unsuppress_name("Header")`
     (vm_typedecl_ops.rs:~499), which deletes the suppression that
     `Cro::HTTP::Header`'s nested grammar registration had installed
     (`suppress_name` in `exec_register_class_op`, parent-is-class branch).
     The suppressed-name branch in `exec_get_bare_word_op`
     (vm_var_get_ops.rs:63-72) — whose comment cites this exact
     `Header.parse` case — therefore no longer fires.
   - the short name `Header` in env ends up bound to the *Router* role's
     package.
3. When a request arrives, `Cro::HTTP::Header.parse` runs (its compiled method
   was found fine), its body's bare `Header` resolves through env to
   `Cro::HTTP::Router::Header`, `.parse` on that role has no user method, so it
   falls into `dispatch_package_parse` (grammar machinery), finds no TOP token,
   and dies "Unknown method value dispatch (fallback disabled): parse". The
   RequestParser's header CATCH converts that to `bad-request('Malformed
   header')`, the supply quits, and no response is written.

gdb backtrace confirming step 3 (frame #12 = the user method executing, frame
#0 = the wrong grammar dispatch):
`dispatch_package_parse(package_name="Cro::HTTP::Router::Header", method="parse")`
← `dispatch_method_by_name_1` ← ... ← `call_compiled_method(receiver="Cro::HTTP::Header", method="parse")`.

## Why the two obvious guards are not enough (both tried, reverted)

- Guarding `unsuppress_name` to bare-name registrations only (so
  `Cro::HTTP::Router::Header` keeps the suppression alive) moves the failure:
  `Cro::HTTP::RequestParser` itself declares `my enum Expecting <RequestLine
  Header Body>` and its `when Header { }` reads bare `Header` as the ENUM
  VALUE. The suppressed-name branch's local-declaration carve-out
  (vm_var_get_ops.rs:73-90) only tolerates a non-Package env value, and by
  then env "Header" holds the Router role's Package (pollution again), so the
  read throws X::Undeclared::Symbols instead.
- Guarding the `"parse"|"subparse"|"parsefile"`-on-Package arm in
  `methods_dispatch_match.rs` with `has_user_method_including_role` is correct
  in itself (a class with its own `parse` method must not fall into grammar
  dispatch) but does not fix this bug — the target has already resolved to the
  wrong type by then.

## The real problem

Short names of nested/package-scoped types are inserted into the ONE global
env at registration time (`exec_register_class_op` short-name binding, role
registration's equivalent), and later modules overwrite or unsuppress them.
Bare type-name resolution inside a method body should be LEXICAL: the
declaring module's own scope (class-body lexicals like `my grammar Header`,
module-body enum values like `Expecting`'s `Header`) must win over another
module's same-short-name type, and each module should see its own view.

That is the same architecture gap as `package_chain_var_fallback` (#5658) but
for TYPE names, with the extra twist that env short-name aliases from
unrelated modules currently WIN over the owner package chain. A candidate
design: on bareword type resolution, probe the current package chain
(`Cro::HTTP::Header::Header` from current_package/method-class stack) BEFORE
the bare env alias, and stop registering short-name aliases in the global env
across module boundaries (keep them per-module, like the parser's scan does).
The suppression set is a partial, name-global approximation of this and keeps
needing exceptions; per-module resolution would subsume it.

## Debugging techniques that worked (reuse these)

- tmp/cro-work is a scratch copy — add `note "DBG: ..."` lines directly to the
  vendored Cro sources to bisect the pipeline stage (TCP accept → conn Supply
  emit → RequestParser packet → req-line → header-line → CATCH).
- The raw socket path is healthy: tmp/raw-tcp-server.p6 (IO::Socket::Async +
  external curl) round-trips fine.
- Make the server script terminate for gdb by giving curl `-m 3`.
- `rust-gdb -batch` breakpoints on the three "Unknown method value dispatch"
  sites (methods_grammar.rs / methods_instance_ops.rs /
  methods_classhow_dispatch.rs) found the guilty one immediately.
