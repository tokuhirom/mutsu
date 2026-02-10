# S02 - Bits and Pieces

Reference: `old-design-docs/S02-bits.pod`

Covers types, literals, variables, and lexical conventions.

---

## Lexical Conventions

### Unicode Semantics
- [ ] NFG (Normal Form Grapheme) formation
- [ ] Unicode horizontal whitespace accepted
- [x] Comments count as whitespace

### Comments
- [x] Single-line comments (`#`)
- [x] Multiline comments (`=begin comment` / `=end comment`)
- [x] Embedded comments (`#`(...)`)
- [ ] User-selected brackets for comments (`#`[...]`, `#`{...}`)

### Whitespace
- [ ] Unspace (`\` + whitespace to suppress whitespace)
- [x] Optional whitespace between constructs
- [ ] Keywords require whitespace after them (disambiguation)
- [x] Implicit topical method calls (`.method` on `$_`)

---

## Built-In Data Types

### Numeric Types
- [x] Int (arbitrary precision concept, currently i64)
- [x] Num (f64)
- [x] Rat (rational number)
- [x] FatRat
- [x] Complex
- [ ] int (native machine integer)
- [ ] num (native floating point)
- [ ] Inf and NaN as first-class values (partial)

### String Types
- [x] Str (Unicode string)
- [ ] str (native immutable string)
- [ ] Buf type (byte buffer)
- [ ] buf8, buf16, buf32, buf64 native buffer types

### Special Types
- [x] Nil
- [x] Bool
- [ ] Whatever (`*`) object with autopriming
- [ ] Mu type (ancestral type)
- [ ] Cool class (coercion base class)
- [x] Junction (any, all, one, none)
- [x] Pair
- [x] Range
- [x] Enum
- [x] Set, Bag, Mix

### Type System
- [ ] P6opaque datatype representation
- [ ] Name equivalence of types (version + authority)
- [x] Type constraints on variables (`my Int $x`)
- [ ] Container types (`is` trait on variables)
- [x] Type objects (undefined prototypes)
- [x] Coercive type declarations (`Int(Str)`)
- [ ] Variables containing undefined values (default trait)
- [ ] `HOW` method (metaclass access)
- [ ] Properties on objects (mixin mechanism)
- [ ] Traits (compile-time properties)
- [ ] Immutable vs mutable type distinction
- [ ] Parametric/generic types
- [ ] Hierarchical types

### Roles (as interfaces)
- [ ] Stringy role
- [ ] Numeric role
- [ ] Real role
- [ ] Integral role
- [ ] Rational role
- [ ] Callable role
- [ ] Positional role
- [ ] Associative role

---

## Names and Variables

### Sigils
- [x] `$` scalar
- [x] `@` array
- [x] `%` hash
- [x] `&` code
- [ ] `::` package/type pseudo-sigil
- [x] Sigils indicate interface (Positional, Associative, Callable)

### Twigils
- [x] `$.foo` public attribute accessor
- [x] `$!foo` private attribute storage
- [x] `$^foo` placeholder positional parameter
- [ ] `$:foo` placeholder named parameter
- [x] `$*foo` dynamic variable
- [x] `$?foo` compile-time variable
- [ ] `$=foo` Pod variable
- [x] `$<foo>` match capture variable
- [ ] `$~foo` sublanguage variable

### Scope Declarators
- [x] `my` lexical scope
- [ ] `our` package scope
- [x] `has` attribute scope
- [ ] `anon` private-to-construct scope
- [x] `state` persistent lexical scope
- [ ] `augment` add to existing name
- [ ] `supersede` replace existing name
- [ ] `unit` compilation-unit scope

### Variable Features
- [x] Invariant sigils
- [ ] List stringification (whitespace-separated)
- [x] `.perl` / `.raku` method
- [x] `.gist` method
- [ ] `.fmt` method (formatted representation)

---

## Subscripts

- [x] Array subscripts `@foo[0]`
- [x] Hash subscripts `%bar{'key'}`
- [x] Hash angle subscripts `%bar<key>`
- [ ] Dot form for explicit dereference (`@foo.[1]`)
- [ ] Subscript adverbs (`:exists`, `:delete`, `:v`, `:kv`, `:p`, `:k`)
- [ ] Combining subscript adverbs
- [ ] Multidimensional slices (`@a[1;2]`)
- [x] List assignment
- [x] Binding (`:=`)

---

## Literals

### Numeric Literals
- [x] Integer literals
- [x] Floating-point literals
- [x] Underscores in numeric literals (`1_000_000`)
- [x] Radix markers (`0b`, `0o`, `0x`)
- [ ] `0d` explicit decimal prefix
- [x] Exponential notation (`1e10`)
- [ ] General radix notation (`:16<DEAD_BEEF>`)
- [ ] Radix with fractional parts (`:16<dead.beef>`)
- [ ] Rational literals (`<1/2>`)
- [ ] Complex literals (`<5+3i>`)
- [ ] Blob literals (`:2{0010_1110}`)
- [x] Imaginary literals (`3i`)

### String Literals
- [x] Single-quoted strings
- [x] Double-quoted strings with interpolation
- [x] Angle-bracket word list `<a b c>`
- [ ] Double angle-bracket word list `<< >>` / `<< >>` with interpolation
- [x] Q/q/qq quoting forms
- [x] Heredoc (`q:to/END/`)
- [ ] Version literals (`v1.2.3`)
- [ ] Allomorphic value semantics (val() processing)

### Interpolation
- [x] Scalar interpolation (`$var`)
- [x] Array interpolation in strings
- [x] Block interpolation (`{expr}`)
- [ ] Method call interpolation in strings (`$obj.method()`)
- [x] Hex interpolation (`\x263a`)
- [x] Octal interpolation (`\o12`)
- [ ] Bracketed hex/octal (`\x[263a]`)
- [ ] Unicode name interpolation (`\c[SNOWMAN]`)

### Adverbial Pairs
- [x] Fatarrow form (`a => True`)
- [x] Colon form (`:a`, `:!a`, `:a(value)`)
- [ ] Colon variable shorthand (`:$a`, `:@a`, `:%a`)

---

## Context

- [x] Sink (void) context
- [x] Item (scalar) context
- [x] List context
- [x] Boolean context (`?`, `.Bool`)
- [x] Numeric context (`+`, `.Numeric`)
- [x] String context (`~`, `.Str`)
- [ ] String `"0"` is now true (differs from Perl 5)
- [ ] Container emptiness for boolean context

---

## Pseudo-packages

- [ ] `MY` (current lexical scope)
- [ ] `OUR` (current package)
- [ ] `CORE` (outermost lexical scope)
- [ ] `GLOBAL` (interpreter-wide)
- [ ] `PROCESS` (process globals)
- [ ] `CALLER` / `CALLERS`
- [ ] `OUTER` / `OUTERS`
- [ ] `DYNAMIC`
- [ ] `UNIT` / `SETTING`
- [ ] Interpolating into names (`::($expr)`)
- [ ] Direct lookup (`Foo::Bar::{&baz}`)
- [ ] Symbol tables access
