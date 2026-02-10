# S04 - Blocks and Statements (Control Flow)

Reference: `old-design-docs/S04-control.pod`

Covers blocks, control flow, phasers, and exception handling.

---

## Blocks and Declarations

- [x] Block closures with parameter declarations
- [x] Pointy blocks (`-> $a, $b { ... }`)
- [x] Placeholder arguments (`$^a`, `$^b`)
- [x] Lexical scoping (visibility from declaration to end of block)
- [x] `state` declarator (persistent lexical)
- [ ] `temp` prefix (temporary value modification)
- [ ] `let` prefix (hypothetical values, restored on failure)
- [ ] `proto` declarator for variables
- [x] `constant` declarator

---

## Conditional Statements

- [x] `if` / `elsif` / `else`
- [x] `unless` (no elsif allowed)
- [ ] Binding with pointy syntax: `if test() -> $a { ... }`
- [ ] Placeholder binding: `if blah() { $^it }`
- [x] Conditional statement modifiers (postfix `if`, `unless`)
- [x] `with` / `orwith` / `without` (definedness checks)
- [x] `with` / `without` statement modifiers

---

## Loop Statements

### while / until
- [x] `while` loop
- [x] `until` loop
- [ ] Binding conditional result: `while something() -> $thing { ... }`

### repeat
- [x] `repeat while` / `repeat until`
- [ ] Binding support in repeat loops

### General loop
- [x] `loop (init; cond; step) { ... }` (C-style)
- [x] `loop { ... }` (infinite loop)

### for
- [x] `for` loop with list iteration
- [x] Loop variable binding: `for @foo -> $item { ... }`
- [x] Multiple parameters: `for %hash.kv -> $key, $value { ... }`
- [ ] `is rw` trait on loop variable
- [ ] Double-ended arrow `<->` for rw parameters
- [x] Implicit `$_` parameter
- [x] Statement modifier `for`

### Loop Control
- [x] `last` (break)
- [x] `next` (continue)
- [x] `redo` (re-run current iteration)
- [x] Labeled loops (`LABEL: for ...`)
- [x] `last LABEL`, `next LABEL`
- [ ] Loop control as methods (`LABEL.next`, `LABEL.last`)
- [ ] Optional return values (`.next($retval)`, `.last($retval)`)

### do-once
- [x] `do { ... }` block as expression
- [ ] `do` statement prefix (returns value of single statement)
- [ ] List comprehensions: `(for 1..100 { ... })`

---

## Switch Statements (given/when)

- [x] `given` statement (topicalize)
- [x] `when` statement (smart match against `$_`)
- [x] `default` case
- [ ] Automatic break from `when` block
- [x] `proceed` statement (go to next when)
- [x] `succeed` statement (break from surrounding block)
- [ ] `succeed($retval)` with return value
- [x] `when` statement modifier

---

## Gather / Take

- [x] `gather { ... }` statement
- [x] `take` function
- [x] Lazy list return from gather
- [ ] `take` context (return value while gathering)
- [ ] State variables inside gather

---

## Other Statement Prefixes

- [x] `try { ... }` (exception trap)
- [ ] `once { ... }` (run once, cache value)
- [ ] `quietly { ... }` (suppress warnings)
- [ ] `start { ... }` (promised computation)
- [ ] `lazy { ... }` (defer evaluation)
- [ ] `sink { ... }` (evaluate eagerly, discard)
- [ ] `eager { ... }` (force eager evaluation)

---

## Exception Handling

### try / CATCH
- [x] `try { ... }` block
- [x] `CATCH { ... }` block
- [x] Smart matching on exception type in CATCH
- [ ] Exception rethrow on unhandled
- [ ] `.handled` property on exceptions
- [ ] Control exception isolation (CATCH vs CONTROL)

### Failure Objects
- [x] `fail` function
- [ ] Failure.defined marks as handled
- [ ] Failure.Bool marks as handled
- [ ] `use fatal` pragma (auto-throw on failure)
- [ ] Sink context auto-throw of unhandled Failures

### Control Exceptions
- [x] `return` (exits surrounding sub/method)
- [ ] `leave` function (exit innermost block)
- [ ] `callframe()` function
- [ ] `goto` statement
- [x] `warn` function (resumable warning)
- [ ] Resumable exceptions with `.resume_value`

---

## Phasers

### Compile-time Phasers
- [x] `BEGIN { ... }` (ASAP, once only)
- [ ] `CHECK { ... }` (ALAP, once only)
- [ ] `LINK { ... }` (link time)
- [ ] `INIT { ... }` (run time, ASAP, once only)

### Block Phasers
- [x] `END { ... }` (program end)
- [x] `ENTER { ... }` (block entry)
- [x] `LEAVE { ... }` (block exit, including unwind)
- [ ] `KEEP { ... }` (successful block exit)
- [ ] `UNDO { ... }` (unsuccessful block exit)
- [ ] `PRE { ... }` (precondition assertion)
- [ ] `POST { ... }` (postcondition assertion)

### Loop Phasers
- [x] `FIRST { ... }` (loop initialization)
- [x] `NEXT { ... }` (loop continuation)
- [x] `LAST { ... }` (loop termination)

### Exception Phasers
- [x] `CATCH { ... }` (catch exceptions)
- [x] `CONTROL { ... }` (catch control exceptions)

### Phaser Features
- [ ] Per-variable traits (`will enter`, `will undo`)
- [ ] Multiple phasers in same block
- [x] Forward order for setup, reverse for teardown
- [ ] `COMPOSE { ... }` (role composed into class)

---

## Statement Parsing

- [x] No parens required for control structures
- [x] Closing brace terminates statement
- [ ] Hash vs block disambiguation rules (complete)

---

## Definition of Success

- [ ] Hypothetical variables (transactional with let/temp)
- [ ] Successful exit returns defined value
- [ ] `fail "message"` explicit failure
- [ ] `return Mu` explicit undefined
