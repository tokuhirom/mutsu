# Assigning `Nil` through an `is rw` method leaves the attribute holding `Nil`, not its type object

Measured 2026-09-06 against raku v2026.07 and a debug `mutsu` built from `main`
at `bbdebb108` plus ADR-0067's returned-container-consumers slice, which does
not touch this path.

```raku
class A { }
class U { has A $.authority is rw; method auth is rw { $!authority } }
my $u = U.new;
$u.auth = A.new;

$u.authority = Nil;   # the ACCESSOR store: raku A, mutsu A   -- correct
$u.auth = Nil;        # the rw-METHOD store: raku A, mutsu Nil -- wrong
say $u.authority.^name;
```

Raku's assignment of `Nil` to a container restores its *default*, which for a
typed attribute with no `is default(...)` is the declared type object. mutsu's
public-accessor store does that; the `is rw` **method** store does not.

## Root cause

`$obj.rwmethod = v` for a method whose body is a bare `$!attr` is served by
`Interpreter::rw_method_attribute_target`
(`src/runtime/methods_mut_rw_attr.rs`) — an AST match that names the exposed
attribute — and the store then writes the value straight into the instance's
attribute map (`methods_mut_method_lvalue.rs`, around the
`attributes.insert(format!("!{}", attr_key), ...)` site). That bypasses the
`Nil` -> declared-default reset (and the `is default(...)` lookup) the ordinary
attribute-store path applies.

The two stores should agree: an `is rw` method that exposes an attribute is an
accessor, so assigning through it must behave exactly like assigning through the
generated accessor. The fix is to route that store through the same
attribute-assignment entry the accessor uses rather than an `insert`, which is a
change to a well-travelled lvalue path and wants its own targeted roast sweep.

## Repro

```raku
class A { }
class U { has A $.a is rw; method acc is rw { $!a } }
my $u = U.new;
$u.acc = A.new;
$u.acc = Nil;
say $u.a.^name;      # raku: A    mutsu: Nil
```
