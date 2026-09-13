# ADR-0101: Grammar rule dynamic variables have a match-scoped stack

- **Status**: Proposed
- **Date**: 2026-09-13
- **Issue**: [#8148](https://github.com/tokuhirom/mutsu/issues/8148)
- **Builds on**: [ADR-0007](0007-grammar-parse-trail-matcher.md), [ADR-0009](0009-regex-code-assertion-execution-model.md), [ADR-0016](0016-span-based-captures-and-lazy-match.md), [ADR-0046](0046-proto-token-ltm-shares-one-ranking-mechanism.md), and [ADR-0073](0073-regex-atom-candidates-are-demand-driven.md)

## 1. Context

A `:my $*x = ...;` declaration in a grammar rule is a dynamic binding for
that rule invocation. It is visible while the declaring rule matches and while
its own action runs. It is not a declaration for every rule in the grammar, and
it must not survive into a sibling rule or the caller after the rule returns.

The current implementation has two separate mechanisms that do not form one
scope:

1. `dispatch_package_parse` scans every rule in the grammar and
   `establish_grammar_dynamic_vars` evaluates all declarations into
   `Interpreter::env` before matching starts. The scan also fills
   `grammar_rule_dynvar_decls`, but the eager evaluation makes the bindings
   parse-wide.
2. A real `RegexAtom::VarDecl` evaluation writes the initializer into the same
   `env`. The value is copied into `RegexCaptures::regex_vars` so the later
   reduce/action walk can re-install it, but the live matcher does not restore
   the binding when a candidate loses or when a subrule returns.

This is observable in a proto-token candidate trial:

```raku
grammar H {
    proto token sigil { * }
    token sigil:sym<dollar> { :my $*LAST = 'dollar'; '$' }
    token sigil:sym<at>     { '@' }
    token TOP { <sigil> }
}
class B { method TOP($/) { make $*LAST // 'none' } }
say H.parse('$', :actions(B)).made;
say H.parse('@', :actions(B)).made;
```

Rakudo returns `none` for both lines. The dollar candidate's declaration is
local to that token, and its value has been left by the time the `TOP` action
runs. mutsu returns `dollar` for both lines: the candidate's declaration is
visible to `TOP`, including when the candidate was only tried and lost.

The existing LTM machinery does not solve this. ADR-0009 and ADR-0046 require
declarative measurement to be side-effect free; `RegexAtom::VarDecl` therefore
uses `LtmAtomMode::SkipZeroWidth`. That protects the measurement pass, but the
subsequent real candidate match still needs an enter/leave scope. ADR-0073 adds
another execution path for demand-driven subrule candidates, so the scope must
be owned by the rule invocation rather than by one ranking implementation.

The already-shipped grammar dynamic-variable use case must remain valid:

```raku
rule TOP { :my %*PLAYED = (); <card>+ }
```

Each `card` invocation must see and mutate the `TOP` binding, while a `card`
that declares its own same-named variable must shadow it only for the duration
of that `card`. The existing pin is
`t/grammar/grammar-reduce-time-dynvar.t`; it must continue to pass.

## 2. Decision

Grammar rule dynamic variables will be represented by a stack of match-scoped
frames. The stack has the following contract:

1. **Enter the declaring rule before resolving and matching its body.** The
   frame is installed before pattern interpolation and remains installed while
   the body, nested subrules, and inline code assertions execute. A declaration
   is initialized once for that invocation, not once during grammar setup.
2. **Restore on every exit path.** A candidate trial restores its frame before
   the next candidate is attempted, before the caller's continuation runs, and
   when matching returns an error. A successful candidate carries a snapshot of
   its declared bindings in its capture node; it does not leave its frame in the
   caller's live environment.
3. **Inherit and shadow normally.** A rule with no declaration reads the nearest
   enclosing frame. A child declaration shadows an enclosing binding. When the
   child returns, the parent frame becomes visible again. Thus a parent-owned
   mutable `%*PLAYED` is shared by sibling children, while a child-owned
   `$*x` is not.
4. **Reduce actions under the same node scope.** The reduce/action walk enters a
   capture node's saved frame, reduces its children, invokes that node's action,
   and restores the prior frame immediately afterward. The flat
   `RegexCaptures::regex_vars` map is not sufficient to describe nested same-key
   declarations; rule-frame metadata must preserve declaration boundaries and
   the value belonging to each node.
5. **The start rule is an ordinary rule invocation.** `Grammar.parse` prepares
   the per-rule declaration table but does not evaluate the union of all
   declarations. The start rule's frame is entered by the same mechanism as a
   subrule, and remains available for its own body and action only. The
   parse-boundary save/restore for the caller remains as a final safety net.
6. **Measurement remains inert.** LTM measurement and longest-prefix
   diagnostics do not enter or initialize rule frames. `RegexAtom::VarDecl`
   remains a zero-width, non-terminating, non-executing atom under
   `LTM_DECLARATIVE_MODE`; `CODE_ATOMS_INERT` remains side-effect free as well.
   Only a real rule invocation may initialize a declaration.
7. **The reduce-time action overlay remains separate.**
   `REGEX_DYNVAR_OVERLAY` represents writes from actions that have already run
   during a live grammar parse and affect later pattern interpolation. It is
   not the ownership model for a rule declaration frame. The two mechanisms
   must be merged only at an explicitly defined rule boundary, not by allowing
   either one to leak through `Interpreter::env`.

The implementation should use one RAII/snapshot abstraction for all rule-entry
paths: eager subrule matching, ADR-0073's streamed path, proto candidate
matching, and `Grammar.parse`'s start rule. The abstraction must save both
environment keys for scalar dynamics (`$*x`'s `*x` and `$*x` aliases), as the
#8096 fix already established for the parse boundary.

## 3. Required implementation slices

The following slices are intentionally ordered so that no partial slice changes
the observable scope contract without a pin:

1. **Pin the semantics.** Add grammar tests covering the issue's dollar/at
   proto-candidate repro, a declaration visible to its own action, a parent
   declaration shared by sibling children, nested same-name shadowing, and
   `@*`/`%*` declarations. Include both a candidate that loses during LTM
   selection and a real candidate that starts matching and then fails.
2. **Introduce the frame abstraction.** Factor save/install/restore, including
   scalar alias pairs, out of the parse-wide setup and the existing dynamic
   parameter helper. Make the no-dynamic-variable path allocation-free or
   effectively so.
3. **Thread frames through real matching.** Enter frames around all named-rule
   candidate bodies and their pattern resolution. Cover both the eager and
   streamed (`ADR-0073`) paths, and ensure a candidate's failed writes cannot
   affect the next candidate or the caller continuation.
4. **Carry boundaries through captures and reduction.** Replace the current
   flat per-rule re-installation with node-local frame metadata that can
   represent nested shadowing. Enter/restore around child reduction and action
   dispatch, including partial-parse and backtracked-reduce replay.
5. **Remove eager union evaluation.** Keep the declaration discovery table, but
   delete the parse-wide initialization of every grammar rule's declaration.
   Re-run the caller-restoration and nested/re-entrant parse tests from #8096.
6. **Run the compatibility matrix.** Verify the focused grammar tests,
   `t/grammar/grammar-reduce-time-dynvar.t`, the regex LTM/proto tests, the
   dynamic-parameter tests, and the full required suites before accepting the
   ADR.

## 4. Acceptance criteria

The implementation is complete only when all of the following hold:

- The #8148 repro returns `none` for both inputs on mutsu, matching Rakudo.
- A losing candidate's initializer is not observable by any sibling or caller.
- A rule's own action sees its own declaration, but an enclosing action runs
  after the child frame has been restored.
- A parent `%*` declaration remains mutable and shared across sibling matches,
  including the existing reduce-time delimiter/card use cases.
- Nested declarations with the same name restore the enclosing value after the
  inner rule returns, for scalar, array, and hash dynamics.
- A failed parse and a failed candidate leave the caller's dynamic variables
  unchanged, including both scalar alias keys.
- LTM and longest-prefix measurement execute no declaration initializer or user
  code, and existing ADR-0009/0046 side-effect pins remain green.
- The eager, streamed, proto, partial-parse, and action-replay paths all obey
  the same enter/leave invariant.

## 5. Non-goals

- This ADR does not redesign the regex engine as a bytecode VM.
- It does not change ordinary lexical `:my`/`:our` declarations in regexes.
- It does not redefine action side effects on backtracking; it only ensures that
  dynamic bindings have the correct extent while those actions execute.
- It does not make a child rule's dynamic binding visible to the caller merely
  because the child matched successfully.
