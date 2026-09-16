# `X::Wrapper` composes for real, and a role's `is built(:bind)` attribute binds

`AttrX::Mooish` (and any distribution following the same pattern) died at
load time with `No such private method 'wrappee-message' for invocant of
type '...'`, raised from inside a `BEGIN { ::?CLASS.^add_role(::('X::Wrapper')) }`
block. Isolating the repro (#8573) turned up three separate, stacked bugs.

## The core `X::Wrapper` role was an empty stand-in

mutsu registers dozens of `X::*` "marker roles" — `X::Comp`, `X::Syntax`,
`X::IO`, and friends — as deliberately empty roles, matching rakudo, purely
so `.^roles`/`~~` agree about which exception classes compose them.
`X::Wrapper` (added to Rakudo's CORE.setting around 2023.10) was lumped into
that list too, but unlike the rest it is NOT empty in rakudo: it is a real,
stateful role (`has Mu $!exception`, `has Mu $!ex-payload`, private methods
`!wrappee-message`/`!is-raku-exception`/`!exception-name-message`, and a
public `exception` method) that lets an exception wrap a lower-level one and
format a message for it — confirmed by introspecting a live `raku`
(`X::Wrapper.^attributes` reports exactly the three attributes the
`AttrX::Mooish::X::Wrapper` bundled polyfill also declares, which exists
precisely to replicate this core role for pre-2023.10 compilers).

`X::Wrapper` is now supplied as a full builtin-role prelude (the same
mechanism `Enumeration`/`Rational`/`IO::Socket` already use — real raku
source, parsed once and spliced into a compunit that mentions the name,
registered through the ordinary `RoleDecl` path), with the actual attributes
and methods. That needed four missing `nqp::` ops the role's method bodies
call: `nqp::ifnull`, `nqp::getpayload`, `nqp::getmessage`, `nqp::backtrace`.
`getpayload`/`backtrace` are safe simplifications (mutsu has no separate
native-exception-with-payload representation, and `Backtrace.new` always
samples the live stack rather than consuming an explicit frame list) —
correct for the paths this composition actually exercises, with `// TODO:`
notes for a future, more faithful implementation.

## A class-registration shell pass validated a role composition too early

mutsu registers every class TWICE: a `__hoisted` forward-reference shell
first (so a later class can be referenced before its own declaration runs),
then the real, source-position declaration. The shell pass does NOT execute
the class body's statements — including a `BEGIN { ... }` that dynamically
composes a role via `.^add_role()` — but it still ran
`validate_private_method_existence`, which checks every `self!method()` call
in the class's declared methods against the private methods actually
composed so far. Since the role hadn't been composed yet in the shell pass,
this raised a spurious `X::Method::NotFound` that aborted the whole class
registration (and hence the whole module load) before the real pass — which
DOES run the `BEGIN` block first — ever got a chance to succeed.
`finalize_class_registration` now skips that validation for the shell pass;
the real declaration re-validates it correctly once the role composition has
actually run.

## A role's `is built(:bind)` attribute never bound from the constructor

`is built(:bind)` is what makes a *private* attribute (`$!x`, not `$.x`)
bindable from a same-named constructor argument at all. mutsu tracked this
per-attribute on `ClassDef::attribute_built`, but `RoleDef` had no equivalent
field: a role's own `has ... is built(:bind)` attribute declaration silently
dropped the trait during role-body registration, and composing that role
into a class never copied anything into the class's `attribute_built` table
either. So `Type.new(exception => $x)` left a role-declared `$!exception`
permanently undefined — exactly the attribute `X::Wrapper` itself relies on.
`RoleDef` now carries its own `attribute_built` map, populated the same way
`ClassDef`'s is, and copied onto the composing class during role
composition.

Regression coverage: `t/exceptions/x-wrapper-role.t` (the builtin role
end-to-end), `t/oo/role/dynamic-add-role-begin-private-method.t` (the
general BEGIN-composed-role-private-method mechanism, independent of
`X::Wrapper`), `t/oo/role/role-attribute-built-bind.t` (the attribute-binding
fix on its own, including the negative case: a role attribute without `is
built` still correctly stays unbound).
