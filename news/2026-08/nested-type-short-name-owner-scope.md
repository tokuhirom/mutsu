# A class's nested type keeps its short name when another module registers the same name

A type declared inside a class body (`class Outer { my grammar Header { ... } }`)
was reachable from `Outer`'s own methods under the bare short name `Header`, but
only until *some other* module registered an unrelated type of the same short
name. After that, the bareword silently resolved to the foreign type.

This is what stalled the Cro battery. `Cro::HTTP::Header` is a class whose body
holds `my grammar Header`, and its `method parse` calls the bare `Header.parse`.
Loading `Cro::HTTP::Router` — which declares `package Cro::HTTP::Router { role
Header { } }` — made every subsequent `Cro::HTTP::Header.parse` dispatch to the
*Router role*, which has no `parse` method, so it fell into grammar dispatch,
found no `TOP` token, and died with "Unknown method value dispatch (fallback
disabled): parse". Cro's request parser caught that as `bad-request('Malformed
header')`, quit the supply, and the HTTP client hung with no response.

## Root cause

Bareword resolution for a nested type's short name went through
`resolve_suppressed_type`, which probes the owner package chain
(`current_package`, the method-class stack, the class under construction) for
`<owner>::<short-name>`. That probe was gated on the name being in
`suppressed_names` — the set that also drives "this bare name is undeclared
outside its owner". Registering a type clears the suppression for its own short
name (`unsuppress_name`), because a fresh declaration must be usable inside its
own body. So an unrelated `Cro::HTTP::Router::Header` registration cleared the
suppression that `Cro::HTTP::Header`'s nested grammar had installed, and with it
the owner-chain probe for *every* class that owned a nested `Header`.

The suppression set was doing double duty: recording undeclared-ness (which is
correctly per-declaration and revocable) and recording "this short name belongs
to some owner package" (which stays true for the rest of the program).

## Fix

Split the second fact into its own set, `class_scoped_short_names`, populated at
the same site that calls `suppress_name` for a class-body-nested type, and never
cleared. `resolve_suppressed_type` now runs its owner-chain probe when the name
is in *either* set, and the bareword opcode no longer pre-gates the call on the
suppression set.

The probe still only succeeds when `<current owner>::<name>` is a real type, so a
scope with no nested type of that name is unaffected: a module-body `my enum
Expecting <RequestLine Header Body>` inside a *different* class still reads
`Header` as its enum value, because that class owns no nested `Header`.

`t/nested-type-short-name-owner-scope.t` pins the behaviour (nine assertions,
passing unmodified under Rakudo): the owner's nested grammar survives a foreign
same-named role, two classes with same-named nested classes each see their own,
and a same-named enum value is not displaced.

## Cro status after this

`Cro::HTTP::Header.parse` now works with `Cro::HTTP::Router` loaded, and Cro's
request parser gets through the request line and all header lines and emits the
request. The end-to-end HTTP round-trip still does not complete; the remaining
blocker is recorded in
`todo/deep/cro-http-request-hang-short-name-env-pollution.md`.
