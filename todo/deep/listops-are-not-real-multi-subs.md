# Core listops (`splice`, `push`, `pop`, ...) are not real multi-subs, so user/module `multi` candidates can't merge with them

## Symptom

Raku treats `splice` (and the other array listops: `push`, `pop`, `shift`,
`unshift`, `append`, `prepend`) as genuine core multi-subs living in the
setting. A user or module `multi sub splice(...)` with a different signature
becomes an *additional candidate* on the same dispatch set — both the core
array form and the new form keep working side by side:

```raku
multi splice(Str $s, Int $i) { "custom $s $i" }
my @a = (1,2,3,4,5);
splice(@a, 1, 2); say @a;      # [1 4 5] — core candidate still dispatches
say splice("x", 1);            # custom x 1 — new candidate also dispatches
```

mutsu does not reproduce this. `splice`/`push`/etc. are parsed as a special
"listop" token class (`is_listop`, `src/parser/primary/ident/listop.rs`,
`src/parser/primary/ident/predicates.rs`) and compiled straight to dedicated
opcodes/native runtime handlers (`src/runtime/methods_mut_dispatch.rs`,
`src/vm/vm_call_method_mut_ops.rs`, `nqp_ops.rs`) — there is no real
`proto`/candidate-list object for them at all. The only accommodation is
`Compiler::user_listop_shadows`
(`src/compiler/helpers_ast_utils.rs::seed_user_listop_shadows`): if the
**current file** has a literal `Stmt::SubDecl`/`Stmt::ProtoDecl` named
`splice` etc., the compiler suppresses the listop rewrite and routes the call
through ordinary user-sub dispatch instead — an all-or-nothing handoff, not a
merge:

```raku
multi splice(Str $s, Int $i) { "custom $s $i" }
my @a = (1,2,3,4,5);
splice(@a, 1, 2); say @a;      # mutsu: "No matching candidates for proto sub: splice"
say splice("x", 1);            # mutsu: custom x 1 (this part alone works)
```

And because `seed_user_listop_shadows` only scans the literal statement list
of the file being compiled, an **imported** multi candidate (`use Module;`
where the module exports `multi splice(...) is export` without its own
`proto sub splice is export`) never triggers the shadow at all — the listop
rewrite still fires, `&splice` from the import is never consulted, and the
call fails outright:

```raku
use String::Splice;             # exports multi splice(Str(Any), ...) candidates,
                                 # proto NOT exported (this is normal/common —
                                 # see below)
say splice('', 0, 'Raku');      # raku: Raku   mutsu: Unknown function: splice
say &splice.defined;            # both: True — the sub exists, just unreachable by name
```

Found via the real-dist compat sweep's `--run-tests` axis on
`String::Splice` (`todo/tickets/dist-test-suite-failures-batch.md`). Note the
method form (`''.splice(0, 'Raku')`) is a red herring — that fails in
**raku** too ("Routine does not have any candidates. Is only the proto
defined?"), so only the plain-sub call form is actually broken in mutsu.

## Why this is deep, not a quick fix

A correct fix needs `splice`/`push`/`pop`/`shift`/`unshift`/`append`/`prepend`
to exist as **real multi-sub objects with real core candidates** registered
in the runtime/compile-time dispatch machinery, so that:

1. A user's local `multi sub splice(...)` in the mainline, and
2. An imported module's exported `multi sub splice(...) is export` (with or
   without its own exported `proto`),

both simply **add candidates** to the existing dispatch set, the same way any
other multi-sub augmentation works elsewhere in mutsu — rather than either
(a) silently doing nothing (current import case) or (b) fully replacing the
builtin behavior (current local-shadow case).

This touches:

- The parser's listop token classification (`is_listop` predicates) — these
  currently short-circuit straight past normal call/multi-dispatch parsing.
- The compiler's dedicated listop compilation path (separate from
  `compile_call`/multi-dispatch compilation).
- The runtime's array-mutation-specific listop handlers
  (`methods_mut_dispatch.rs`, `vm_call_method_mut_ops.rs`, `nqp_ops.rs`),
  which operate directly on array/string containers rather than going through
  generic multi-candidate matching.
- `Compiler::user_listop_shadows` and its seeding, which would need to become
  "does any candidate exist for this name" (local OR imported) rather than
  "does this file declare it", and the fallback would need to become "try the
  merged candidate set" rather than "use only the user's candidates".

None of this is a small, localized change — it is closer in shape to giving
listops a first-class place in the same proto/candidate machinery used for
ordinary `multi sub`s, while keeping their existing fast native execution
path for the core (array/string) candidates so there is no performance
regression on the hot `push`/`pop`/`splice` paths that don't involve any
user/module extension. Needs a design pass on how the fast native path and
the generic multi-dispatch path share one candidate set before implementation
starts.

## Also worth checking once this is designed

- Whether the same issue applies to any other "listop-shaped" builtin beyond
  the array/string mutators listed above.
- Whether raku's actual behavior additionally requires the imported/local
  extra candidate to see the *narrowed* invocant types correctly relative to
  the core candidates' `Positional`/`Str` signatures (ambiguity/precedence
  rules for multi dispatch), not just "does it dispatch at all".
