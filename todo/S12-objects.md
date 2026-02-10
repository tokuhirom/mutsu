# S12 - Objects

Reference: `old-design-docs/S12-objects.pod`

Covers classes, methods, attributes, inheritance, multi-dispatch, and introspection.

---

## Classes

- [x] `class Name { ... }` declaration
- [ ] `unit class Name;` file-scoped declaration
- [x] Type objects (undefined prototypes)
- [ ] Forward declaration with yada: `class A { ... }`
- [x] Single inheritance (`is Parent`)
- [x] Multiple inheritance (`is Parent1 is Parent2`)
- [x] Role composition (`does Role`)
- [ ] `also is`, `also does` inside class body
- [ ] Anonymous class: `class :: is Parent { ... }`
- [ ] `my class` for lexical scope
- [ ] Autoboxing (derive from built-in types)
- [ ] Closed / finalized classes

---

## Metaclasses

- [ ] `HOW` method (metaclass access)
- [ ] Type object vs instance distinction via `.defined`
- [ ] Custom meta-objects (`is MetaClass`)

---

## Methods

### Declaration
- [x] `method name { body }`
- [x] `method name(params) { body }`
- [ ] Optional invocant declaration: `method foo($self: $a)`
- [x] `self` keyword
- [ ] `submethod` (non-inheritable method)

### Private Methods
- [ ] `method !private_method { ... }`
- [ ] Called via `self!method` or `$obj!method`

### Method Calls
- [x] Dot notation: `$obj.method(args)`
- [ ] Indirect: `method $obj: args`
- [x] Implicit `$_` method call: `.method`
- [ ] Quoted method names: `$obj."$name"()`
- [ ] Operator calls: `$obj.infix:<*>($y)`

### Fancy Method Dispatch
- [ ] `.?meth` call if exists, else Nil
- [ ] `.*meth` call all (0+ results)
- [ ] `.+meth` call all (1+ or die)
- [ ] `callsame` / `callwith()` (re-dispatch)
- [ ] `nextsame` / `nextwith()` (tail call re-dispatch)
- [ ] `samewith()` (same dispatch, different args)
- [ ] `lastcall` (stop dispatch chain)

### Lvalue Methods
- [ ] `is rw` trait on methods
- [ ] Usable with `temp` / `let`

### FALLBACK
- [ ] `method FALLBACK($name, |args)` for missing methods

---

## Attributes

- [x] `has $.name` (public with accessor)
- [x] `has $!private` (private, no accessor)
- [x] Default values: `has $.name = "default"`
- [ ] `has $.name is rw` (read-write accessor)
- [ ] Class attributes (`our $.shared`, `my $.lexical`)
- [ ] Initializers run in declaration order
- [ ] `BUILD` time evaluation of defaults

---

## Construction and Initialization

- [x] Default `new` from `Mu`
- [x] Named arguments auto-pass to attributes
- [x] `BUILD` submethod
- [x] `TWEAK` submethod
- [ ] `bless` method for custom construction
- [ ] Representation types: `P6opaque`, `P6hash`, etc.
- [ ] `.clone(:attr<new>)` cloning with modifications

---

## Multi Methods

- [x] `multi method name(Type $x) { ... }`
- [x] Multiple candidates with different arities
- [ ] Full type-based narrowness sorting
- [ ] Tiebreakers: scope preference, declaration order, `is default`
- [ ] Constrained type candidates (`where` clauses in dispatch)

---

## Parallel Dispatch (Hyper Methods)

- [ ] `@obj>>.method()` call on each element
- [ ] `@obj>>.?method()`, `@obj>>.*method()` variants
- [ ] `@obj>>.=method()` mutator on each

---

## Delegation

- [ ] `handles` trait on attributes
- [ ] `has $!tail handles 'wag'`
- [ ] Smartmatch selectors: `handles /^get_/`
- [ ] Wildcard: `handles *`, `handles **`
- [ ] Renaming via pairs/hash

---

## Types and Subtypes

- [x] `subset Name of Type where predicate`
- [ ] Type adverbials: `:D` (defined), `:U` (undefined), `:_` (any)
- [ ] Multiple `where` constraints
- [ ] Static `where` as part of official type

---

## Enumerations

- [x] `enum Name <variant1 variant2 ...>`
- [x] `enum Name (key => value, ...)`
- [ ] `.pair`, `.key`, `.value` methods
- [ ] Anonymous enumerations
- [ ] `$obj but Enum` mixin
- [ ] `is export` on enums
- [ ] `.pick` method on enum type

---

## Trusts

- [ ] `trusts OtherClass` declaration
- [ ] Cross-class private method access

---

## Introspection

- [x] `.WHAT` (type object)
- [ ] `.HOW` (metaclass)
- [ ] `.WHO` (package)
- [x] `.isa(Type)` type check
- [ ] `.does(Role)` role check
- [ ] `.can('method')` method check
- [x] `.defined` (instance vs type object)
- [ ] `.Meta` (meta information)

---

## MRO (Method Resolution Order)

- [x] C3 linearization
- [ ] `.^mro` method for MRO list
- [ ] Multiple inheritance resolution

---

## Scalar Container Indirection

- [ ] Method calls on mutable scalars go to contained object
- [ ] `VAR` prefix for container access
- [ ] Autoboxing for non-scalar containers
