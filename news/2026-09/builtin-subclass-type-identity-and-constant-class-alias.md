# An `is Array` subclass keeps its own type identity, and a `constant` class alias names its class

`todo/tickets/imported-constant-class-alias-does-not-resolve.md` reported that a
module giving a verbosely-named class a short alias — a very common Raku
packaging idiom —

```raku
unit module RoundedMod;
class Array::Rounded is Array is export {}
my constant Rounded is export = Array::Rounded;
```

resolved to the *base* class in the consumer, both as a term and in an `is`
trait: `Rounded.new(1).^name` and `my @a is Rounded` both answered `Array` where
raku answers `RoundedMod::Array::Rounded`.

## Neither half was about the constant, and the ticket's split was wrong

The ticket framed this as one problem in two resolution paths and said the
bareword path "*does* resolve a same-file `my constant` alias correctly". It
does not — `class Array::Rounded is Array {}; my constant Rounded = Array::Rounded;
Rounded.new(1).^name` answers `Array` with no module and no `::` in sight. Nor
is the `::` in the name load-bearing: `class R is Array {}; say R.new.^name`
answers `Array` too, while `class R is Hash {}` answers `R`. Reduced, the first
half has nothing to do with aliases at all.

### Half 1: the Array-subclass delegation swallowed type identity

An `is Array` / `is List` subclass keeps its elements in a backing
`__mutsu_array_storage` attribute, and `exec_call_method_op_impl` delegates
**every** method the class does not define itself to that plain `Array`. That is
right for the Positional protocol and for rendering — raku really does answer
`[1 2]` for `.gist` and `1 2` for `.Str` — but wrong for every method that
reports *what the receiver is*. Measured against raku v2026.06:

| | raku | mutsu (before) |
|---|---|---|
| `R.new(1,2).^name` | `R` | `Array` |
| `.WHAT` | `(R)` | `(Array)` |
| `.isa(R)` / `.does(R)` | `True` | `False` |
| `.^parents.head` | `(Array)` | `(List)` |
| `.WHICH` | an `ObjAt` on `R` | one on `Array` |

Going through a variable was already correct, because the `CallMethodMut` path
does not take this delegation — so the two spellings of the same call disagreed.

The Associative twin never had the bug: `try_hash_storage_delegate` delegates
through a curated allowlist (`is_hash_storage_method`) that simply omits `.WHAT`,
`isa` and `does`. The fix states the same rule as an exclusion, since the Array
side deliberately delegates by default: `Interpreter::is_type_identity_method`
holds back `WHAT`, `WHICH`, `isa`, `does`, and every `^`-prefixed name (a `HOW`
meta-method describes the type by construction), at both delegation sites.

### Half 2: `is Alias` probed the registry with the literal trait name

`exec_apply_var_trait_op` receives the trait name exactly as written, baked into
the constant pool, and gates the "tie this variable to that class" branches on
`registry().classes.contains_key(trait_name)`. A `constant` bound to a type
object misses every probe, so `my @a is C` silently left an ordinary `Array`.

`trait_name_through_constant_alias` resolves such a name through its binding,
but only when the literal name is **not** itself a registered class or role and
the name it is bound to **is** one. It runs *below* every built-in variable
trait, not beside `lexical_env_remap_name` at the top of the op: built-in trait
names (`default`, `rw`, …) arrive through the same parameter, and an unrelated
lexical can bind one of them to a type object — `my $default = Int` aliases the
bare name `default` — which would rename the built-in trait out from under its
own branch. That is the exact trap `lexical_env_remap_name`'s `\0` guard exists
for, and `t/builtin-subclass-type-identity.t`'s last assertion pins it.

## Coverage

`t/builtin-subclass-type-identity.t` — 20 assertions, all dual-oracled against
raku, with fixture `t/lib/RoundedMod.rakumod`: type identity in both call forms,
the Positional protocol still delegating, the same-file and cross-module
constant-alias spellings of both the term and the `is` trait, and the
`default`-shadowing guard. `make test` (3643 files) and a 598-file targeted roast
sweep are green.

`Array::Rounded`'s row in `dist-test-suite-failures-batch.md` had these as its
remaining blockers, alongside the postcircumfix half fixed the same day
(`news/2026-09/core-postcircumfix-subscript-routine.md`).

## Left open

`my $v = R.new(1, 2); $v.Str` still answers `R()` where raku answers `1 2` — the
`CallMethodMut` delegation's allowlist (`is_array_storage_native_safe`) does not
carry the rendering methods, the mirror image of half 1. Filed as
`todo/tickets/array-subclass-mut-path-does-not-delegate-rendering.md`.
