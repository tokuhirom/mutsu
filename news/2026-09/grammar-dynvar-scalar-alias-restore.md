# A grammar `:my $*VAR` no longer leaks past `.parse` into the caller's scope

`:my $*VAR = …;` inside a grammar rule body declares a dynamic variable, and
`establish_grammar_dynamic_vars` (`src/runtime/methods_grammar.rs`) evaluates such declarations
into `self.env` before the parse starts, saving the prior value so `.parse`'s caller sees it
restored afterward. The save/restore was keyed on `dynamic_decl_var_key`'s result directly — which
keeps the `$` sigil (`$*LAST`) — while the actual runtime storage for a `$*`-sigil dynamic variable
lives under a bare-name/sigil-kept **alias pair** kept in sync by
`set_env_with_main_alias_inner`'s `twigil_dynamic_alias` (`*LAST` and `$*LAST` both hold the same
value, updated together). Saving and restoring only the sigil-kept half left the bare half's value
in place, so a `rust-gdb` trace on every `Env::insert` of the variable's key showed the restore loop
correctly clearing `*LAST` while `$*LAST` (the half a plain `$*LAST` read actually resolves through)
kept its stale value — the observable leak.

The fix tracks and restores both halves of the pair for a `$`-sigil dynamic variable.  `@*`/`%*`
dynamic variables have no such pair (only ever one, sigil-kept key) and are unaffected.

```raku
grammar H {
    token sigil:sym<dollar> { :my $*LAST = 'dollar'; '$' }
    ...
}
H.parse('$', :actions(B));
say $*LAST // 'unset';   # mutsu (before): dollar   raku / mutsu (after): unset
```

A narrower, deeper half of the original report — the same declaration is also visible, *during* the
parse, to actions of sibling rules that never matched at all (an LTM candidate that loses still runs
its `:my` declaration) — needed real per-rule dynamic-scope tracking through the token-call/LTM
candidate machinery, which is a cross-cutting change, not a local fix; filed separately as #8148.

Fixes #8096 (the leaks-past-`.parse` half).
