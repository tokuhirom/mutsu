# ADR-15519: Support the core Awaitable::Handle protocol

- **Status**: Proposed; the undocumented core-protocol exception requires the maintainer's decision.
- **Date**: 2026-11-01
- **Issue**: [#15519](https://github.com/tokuhirom/mutsu/issues/15519)
- **Related**: `AGENTS.md`'s core-routine documentation rule; the existing native Promise/Channel await paths.

## 1. Question

Should mutsu support Rakudo's core `Awaitable::Handle` role, including its
undocumented protocol methods, rather than treating it as outside the supported
core surface? The core-routine rule admits functions documented under
`raku-doc/doc/Type/`; this role and its protocol members have no such entry in
the vendored documentation. This proposal requests an explicit exception for
this protocol, not a general admission rule for undocumented functions.

Async::Workers 0.3.1 explicitly composes the role and declares a
`get-await-handle(--> Awaitable::Handle:D)` return constraint. Merely making
`await` accept additional objects would not make that source compile: the role
must exist and participate in composition and type checks.

## 2. Measured contract

Rakudo 2026.07, without an import, reports:

```text
Awaitable::Handle.HOW.^name
    Perl6::Metamodel::ParametricRoleGroupHOW
Awaitable::Handle.^attributes
    $!already:Bool, $!success:Bool, $!result:Mu, $!cause:Exception
Awaitable::Handle.^methods(:local)
    already-success, already-failure, subscribe-awaiter
```

The methods' source location is `SETTING::src/core.c/Awaitable.rakumod`.
`already-success(Mu \result)` constructs a handle of the invocant's class
with `already` and `success` true and the supplied result. The unused cause
is the `Exception` type object. `already-failure(Mu \cause)` constructs one
with `already` true, `success` false, the supplied exception and a `Mu` result.
The four attributes supply accessors on the composed class.

A class composing the role must implement `subscribe-awaiter(&subscriber)`:

```raku
class H does Awaitable::Handle { }
# Rakudo: Method 'subscribe-awaiter' must be implemented by H
# mutsu: X::InvalidType: Invalid typename 'Awaitable::Handle'
```

The issue's informal list of members is therefore not the implementation
contract: `already` and `result` are attributes' accessors, and the role's
three declared methods are the ones above.

## 3. Proposed decision

Support this specific core protocol, with a real role definition and its actual
state, factories and required method. Preserve role identity and nominal
`Awaitable::Handle:D` constraints. An empty marker role, an alias to `Promise`,
or a special case for Async::Workers would not implement this contract.

Use the normal core-prelude role registration and compiler/VM method path for
the role's Raku definitions, following the existing stateful core-role model.
The factories and accessors must be executable through the ordinary compiled
method machinery. New protocol calls must not extend the legacy
`call_method_with_values` slow path.

Implement and validate in two independently reviewable units:

1. Register the complete role contract. Pin composition, required-method
   rejection, attribute accessors, both factories, subclass construction and
   nominal return constraints against Rakudo. Re-run the real Async::Workers
   load with the same dependency paths under both interpreters; record any
   further blocker independently.
2. Make `await` consume the handle protocol as well as its existing native
   targets. An already-settled successful handle yields its result; a failed
   one raises the matching await failure; a pending handle uses its subscriber
   without losing completion or delivering it twice. Promise/Channel handle
   adapters must satisfy the same nominal role. Keep the existing native
   settlement machinery shared rather than implementing a second Promise or
   Channel engine.

Today `await_normalize` obtains `get-await-handle` only for non-native
Awaitables, and `builtin_await` recognizes native Promise/Channel values and
otherwise returns its target unchanged. `Promise.get-await-handle` returns
the native Promise itself. Registering the role alone does not establish the
full await protocol; the second unit must be measured and tracked explicitly.

## 4. Acceptance and scope

The role has Rakudo's identity, fields and method contract, and explicit
composition and return constraints work. Native and user-defined handles
settle through the same await contract, including rejection and callback
ordering. Focused regressions cover both the already-settled and pending
paths. The real distribution's load is measured before an implementation PR
claims to resolve #15519.

Distribution test files that require the legacy-only Test::Async remain
`no_baseline` under the repository's RakuAST-oracle policy. Supporting this
core protocol does not authorize a native replacement for an ecosystem module
or a general structural/duck-typed substitute for named roles.
