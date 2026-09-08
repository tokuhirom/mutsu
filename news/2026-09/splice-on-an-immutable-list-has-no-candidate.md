# `splice` on an immutable List resolves no candidate, it is not an immutability error

Six of the seven list mutators are declared on `List`, so calling one on an
immutable invocant reaches an immutability check and dies with `X::Immutable`:

```
my $l = (1,2,3); $l.push(9)    X::Immutable  "Cannot call 'push' on an immutable 'List'"
my $r = 1..3;    $r.push(9)    X::Immutable  "Cannot call 'push' on an immutable 'Range'"
```

`splice` is the seventh, and it is not one of them. Rakudo declares `splice` on
`Array` only, so a `List` or `Range` invocant resolves no candidate at all and
never reaches an immutability check:

```
my $l = (1,2,3); $l.splice(0,1)
    X::Multi::NoMatch
    "Cannot resolve caller splice(List:D, Int:D, Int:D); Routine does not have
     any candidates.  Is only the proto defined?"
```

mutsu routed all seven through the same `X::Immutable` arm, in both the
value-path (`methods_call_dispatch.rs`) and the lvalue-path
(`methods_mut_dispatch.rs`) dispatchers, so `splice` came out with the wrong
exception class *and* the wrong message.

That is not cosmetic. It is one of the two remaining residues of the
`X::OutOfRange` / `CATCH` descent in the `Config::TOML` battery ticket
([#7539](https://github.com/tokuhirom/mutsu/issues/7539)): `Crane`'s `add`
descent catches the failure and maps it to `X::Crane::Add::RO`, and it matches
on rakudo's spelling, so under mutsu the `CATCH` fell through and the wrong
exception escaped.

`make_no_candidates_error` renders rakudo's wording — note the two spaces before
"Is only the proto defined?" — from the invocant and the actual arguments, each
as its type name plus a `:D`/`:U` smiley, so the signature tracks both the arity
and the argument types (`splice(List:D, Str:D)` for `$l.splice("x")`). It sits
beside `make_multi_no_match_error`, which is the *different* `X::Multi::NoMatch`
case: a routine that has candidates and none of them bound ("none of these
signatures matches").

Pinned by `t/splice-on-immutable-list-has-no-candidate.t`, 17 assertions passing
unchanged under `raku` as well as under mutsu: the no-candidate rows across five
arities and both invocant types, the six mutators that must still answer
`X::Immutable`, and `splice` on a real `Array` — including the `*-0` start,
whose "inserts at index 0 instead of at the end" symptom the same ticket
records. That symptom did not reproduce on re-measurement; `@a.splice(*-0, 0, 9)`
appends correctly, so only the exception-class residue was live.
