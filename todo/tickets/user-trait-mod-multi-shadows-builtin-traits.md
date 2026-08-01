# A user `trait_mod:<is>` multi shadows every built-in trait

rakudo's `Test.rakumod` exports one:

```raku
multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) is export { ... }
```

In Raku that candidate simply joins the multi; the built-in candidates are still
there, so an unrecognised trait still reaches the fallback that raises
`X::Comp::Trait::Unknown`. In mutsu, importing it makes **every** `is` trait
dispatch through user multi-dispatch, and an unknown trait comes back as the
dispatch failure instead:

```
# with `use Test2;` (the aliased upstream Test.rakumod) in scope
my $a is definitely-invalid = 5;
# mutsu: X::Multi::NoMatch -- Cannot resolve caller trait_mod:<is>(Any:D);
#        none of these signatures matches: ...
# raku:  X::Comp::Trait::Unknown -- Unknown variable trait 'is definitely-invalid'
```

Without the import mutsu gets it right, so the built-in path is fine; what is
missing is the fallback *from* user dispatch back to it. A user `trait_mod:<is>`
that matches nothing should not consume the trait.

Reproduce with `mutsu -I tmp/core -e 'use Test2; my $a is definitely-invalid = 5'`
(see the vendoring ticket for how `tmp/core/Test2.rakumod` is produced), or under
the alias with `t/variable-traits.t`, which this keeps red under the real module
even after `news/2026-08/pod-begin-at-end-of-input.md` corrected the file's own
`lives-ok` bug.

The same shape presumably applies to `trait_mod:<does>` / `<of>` / `<returns>`
and to a user `trait_mod:<is>` from any other module — `Test` is just the one
that surfaced it.
