# ADR-0019 E6d closes: `ArrayPush`'s proposed augmented-Array guard was solving a non-problem

E6's design doc flagged one open verification item (V2) for the `ArrayPush` fast-path
opcode: does `augment class Array { method push(...) }` diverge from the opcode's
direct-Arc-append shortcut? The proposed fix was a generation-refreshed
`array_dispatch_pristine` bit that would fall `ArrayPush` back to full method dispatch
whenever any user/wrap row existed under `Array`/`List`.

Running the actual raku baseline first (per this campaign's standing rule — measure
before naming the fix) showed the premise doesn't hold:

```raku
use MONKEY-TYPING;
augment class Array { method push($x) { say "USER-PUSH: $x"; self } }
# raku: ===SORRY!=== Package 'Array' already has a method 'push'
#       (did you mean to declare a multi method?)
```

Both the plain and `multi method` forms are illegal in raku, on both `Array` and its
parent `List` — the same "illegal program" shape ADR-0019's E5b step 2 already found for
`augment class Str { method uc {...} }`, and explicitly declined to fix, since raku
itself rejects the program before any dispatch-ordering question can arise. mutsu
silently accepts the same illegal augment (a separate, pre-existing, general
compile-time gap — missing redeclaration/multi-ambiguity detection for `augment` — not a
dispatch-ordering defect), so `ArrayPush`'s bypass of it is not a new bug.

The one *legal* way to override `.push` on an array value — a `does`-mixin — already
works correctly today, with no code change:

```raku
role Loud { method push($x) { say "ROLE-PUSH: $x"; self } }
my @a = (1, 2, 3);
@a does Loud;
@a.push(4);   # raku and mutsu: identical, "ROLE-PUSH: 4" then [1 2 3]
```

`exec_array_push_op`'s existing `is_simple_array` gate already excludes a mixed-in
array — `@a does Loud` rebinds `@a` away from a plain `ValueView::Array`, so the fast
path never even runs and the call falls through to the always-correct generic dispatch.
This is the third time this campaign has found "the receiver-shape check the fast path
already has to make IS the safety net" — after `CallMethod`'s native probe (E5b step 2)
and the augmented native collection methods fix (E6b step 2) — now confirmed at a
completely different, much simpler opcode.

Conclusion: the proposed `array_dispatch_pristine` bit is not needed. Building it would
add a permanent per-push registry-generation check to defend against a divergence that
does not exist for any legal raku program. E6a, E6b, and E6d are now closed; E6c (the two
dynamic-call gaps) is the only remaining open box in ADR-0019 Phase E's E6.
