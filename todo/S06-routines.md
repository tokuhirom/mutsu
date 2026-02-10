# S06 - Subroutines and Routines

Reference: `old-design-docs/S06-routines.pod`

Covers subroutine declarations, parameters, signatures, and advanced features.

---

## Code Object Types

- [x] `sub` (subroutine - non-inheritable routine)
- [x] `method` (inheritable routine with invocant)
- [ ] `submethod` (non-inheritable method)
- [x] `regex` (pattern matching method)
- [x] `token` (non-backtracking regex)
- [x] `rule` (non-backtracking + sigspace regex)
- [ ] `macro` (compile-time routine)
- [x] Blocks (bare curlies) with `$_`
- [x] Pointy blocks (`-> $a { ... }`)
- [ ] Thunks (code that may not execute immediately)

---

## Routine Modifiers

- [x] `multi` (multiple dispatch variants)
- [x] `proto` (prototype for multi variants)
- [ ] `only` (explicit single routine, default)
- [ ] `dispatch` keyword (internal dispatcher)
- [ ] Proto body with `{*}` placeholder
- [ ] `is default` trait for default multi
- [ ] Auto-generation of proto when none found

---

## Subroutine Declarations

### Named Subroutines
- [x] `sub name { body }`
- [x] `sub name(params) { body }`
- [x] Return type specification
- [ ] `my` scope (default, lexical)
- [ ] `our` scope (package-visible)
- [ ] Post-declaration of subroutines

### Anonymous Subroutines
- [x] `sub (params) { body }`
- [x] Lambda / pointy block: `-> $a { ... }`
- [ ] `anon` scope modifier

### Stub Declarations
- [ ] Yada operators: `...`, `!!!`, `???`
- [ ] Predeclaration without definition

### Lvalue Subroutines
- [ ] `is rw` trait for proxy returns
- [ ] Proxy objects with FETCH/STORE

### Operator Overloading
- [ ] Grammar category prefix: `prefix`, `postfix`, `infix`, `circumfix`, `postcircumfix`
- [ ] Operator name syntax: `category:<OPNAME>`
- [ ] Lexical scoping of syntactic effects
- [ ] Unicode operator support

---

## Signatures

### Basic Syntax
- [x] Zero or more parameter declarations
- [x] Parenthesized parameter list
- [x] Return specification with `-->` token
- [x] Implicit return from final statement
- [x] Explicit `return`

### Parameter Types
- [x] Positional parameters (required)
- [x] Optional parameters (with `?` or default value)
- [x] Named parameters (`:$name`)
- [x] Slurpy positional (`*@args`)
- [x] Slurpy named (`*%opts`)
- [ ] Slurpy scalar (`*$head`)
- [ ] Capture parameter (`|args`)
- [ ] Term binding (`\term`)
- [ ] Multidimensional slurpy (`**@slice`)
- [ ] One-argument slurpy (`+@args`)

### Parameter Traits
- [ ] `is readonly` (default for most params)
- [ ] `is rw` (modification allowed)
- [ ] `is copy` (pass by value)
- [ ] `is raw` (lazy contextualization)
- [ ] `is dynamic` (environmental variable)

### Parameter Features
- [x] Default values with `=`
- [x] Type constraints (`Int $x`)
- [ ] Type smileys (`:D`, `:U`, `:_`)
- [ ] `where` constraints on parameters
- [x] Named parameter aliases
- [ ] Unpacking array parameters (`[$a, *@rest]`)
- [ ] Unpacking hash parameters
- [ ] Attributive parameters (`$.name`, `$!name`)
- [x] Placeholder variables (`$^a`, `$^b`)
- [ ] Placeholder named variables (`$:name`)

### Invocant
- [ ] Explicit invocant with colon (`method foo($self: $a, $b)`)
- [ ] `self` as implicit invocant
- [ ] Indirect object syntax

---

## Calling Conventions

- [x] Positional arguments
- [x] Named arguments (`:name(value)`, `name => value`)
- [ ] `|` prefix for flattening into arglist
- [ ] Pair handling as named vs positional
- [ ] Autogeneration of pairs (`:$variable`)

---

## Advanced Features

### The `return` Function
- [x] Return from subroutine
- [ ] Return as control exception
- [ ] Named return values

### `callframe` and `caller`
- [ ] Frame navigation and matching
- [ ] `.my` method for lexical namespace access
- [ ] `.file`, `.line` properties

### Temporization
- [ ] `temp` macro for temporary value replacement
- [ ] `.TEMP` method invocation
- [ ] Closure restoration at scope end

### Wrapping
- [ ] `.wrap` method on Routine objects
- [ ] `callsame` / `callwith` functions
- [ ] `nextsame` / `nextwith` tail calls
- [ ] Wrap handle restoration
- [ ] Scoped wrapping with temporization

### Special Variables
- [x] `&?ROUTINE` (current routine reference)
- [x] `&?BLOCK` (current block reference)

### Priming
- [ ] `.assuming` method (partial function application)
- [ ] `*` for argument skipping in priming

### Macros
- [ ] Compiler-called functions
- [ ] `is parsed` trait
- [ ] Quasiquoting
- [ ] AST-based returns

---

## MAIN Subroutine

- [x] `MAIN` sub for script entry
- [ ] Command-line argument Capture conversion
- [ ] Multi dispatch for MAIN variants
- [ ] `unit` declarator for semicolon form
- [ ] USAGE routine / auto-generated usage
- [ ] `$*USAGE` message
- [ ] Unix command-line convention mapping

---

## Signature Introspection

- [ ] `.params` method for Parameter list
- [ ] Parameter properties: name, type, constraints, readonly, rw, etc.
- [ ] `.signature` on routines
- [ ] `.cando($capture)` for dispatch checking
- [ ] `.candidates` for multi candidate list

---

## Feed Operators

- [ ] `==>` rightward feed
- [ ] `<==` leftward feed
- [ ] `==>>` append feed
- [ ] `<<==` append leftward feed
- [ ] Lazy vs eager evaluation with feeds
