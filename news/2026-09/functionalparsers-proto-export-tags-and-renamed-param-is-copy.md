# Proto export tags and renamed-parameter `is copy` leaks

Investigating FunctionalParsers 0.1.10's EBNF grammar pipeline (#8526)
surfaced two general dispatch/binding bugs, unrelated to grammars as such —
they show up wherever a distribution exports a `proto` with an explicit tag,
or forwards a renamed named parameter across a call.

**`proto NAME(|) is export(:TAG, :ALL)` dropped every tag but DEFAULT.**
`Stmt::ProtoDecl` had no `export_tags` field at all — the AST simply never
recorded a proto's tag list, so `exec_register_proto_sub_op` always exported
under an empty tag set, which defaults to `["DEFAULT"]`. `use Mod :TAG` then
saw the whole multi family's tags as `{"DEFAULT"}`, found that disjoint from
the requested `{"TAG"}`, and silently imported nothing: the proto ended up
with zero candidates, so any call raised "Cannot resolve caller NAME(...);
none of these signatures matches". `export_tags` is now threaded through the
AST, the compiled proto-decl plan, and the registration op, matching how an
ordinary `sub … is export(:TAG)` already worked.

**A renamed named parameter's `is copy`/`is rw`/`is raw` never reached its
own readonly mark.** `:target(:$actions) is copy` parses the trait onto the
alias WRAPPER (external key `target`), never onto the LEAF variable it binds
(`$actions`) — a body only ever sees `$actions`, never `$target`. The
signature binder's readonly-marking loop, though, walks only the top-level
parameter list, so for a renamed param it marked/unmarked `target`, a symbol
nothing ever reads or assigns. `actions` was left exactly as the shared,
symbol-keyed readonly table already had it. That is invisible until the
CALLER happens to declare its own unrelated `:$actions` parameter: the
caller's frame marks bare symbol `actions` readonly, and — because the
callee's `is copy` never reached that symbol — the mark leaks straight
through into the callee's `$actions`, which then fails "Cannot assign to a
readonly variable" despite its own `is copy`. `bind_named_rename_sub_signature`
now marks/unmarks the actual leaf, using the wrapper's traits threaded down
through the (possibly chained) alias recursion.

Together these fixed two of FunctionalParsers' baseline files:
`t/10-shorcuts.rakutest` (full pass) and `t/17-grammar-graph.rakutest`
(a hard crash to reaching its test plan), moving the distribution's ledger
record from 6/18 to 7/18 parity files and from four regressions to two.

Pinned by `t/modules/import-export/proto-export-tag.t` and
`t/routines/signature/named-param-rename-is-copy-readonly-leak.t`.

The remaining FunctionalParsers gap — Nil/empty grammar-action results and
two `No such method 'parser' for invocant of type 'Any'` deaths — is a
separate, deeper problem tracked by #8526 and a new follow-up issue.
