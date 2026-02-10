# mutsu - Roadmap

Goal: Build a practical Raku (Perl 6) runtime in Rust that is faster than MoarVM.

---

## Phase 1: Language Core (current)

Get minimal Raku programs running. Prioritize feature implementation.

### Type System
- [x] Int
- [x] Num (f64)
- [x] Str
- [x] Bool
- [x] Array
- [x] Hash
- [x] Range (`..`, `..^`)
- [x] Pair
- [x] FatRat
- [x] Nil
- [x] Rat (rational number)
- [x] Complex
- [x] Set, Bag, Mix
- [x] Enum
- [x] Junction

### Literals
- [x] Integer literals
- [x] Floating-point literals
- [x] Single-quoted strings
- [x] Double-quoted strings + variable interpolation
- [x] Angle-bracket word list `<a b c>`
- [x] Underscores in numeric literals (`1_000_000`)
- [x] Radix notation (`0x`, `0o`, `0b`)
- [x] Exponential notation (`1e10`)
- [x] Q/q/qq forms
- [x] Heredoc (`q:to/END/`)
- [x] Regex literals

### Variables
- [x] `$` scalar
- [x] `@` array
- [x] `%` hash
- [x] `$_` topic variable
- [x] `$!` error variable
- [x] `$*` dynamic variables (`$*PID`, `$*CWD`, etc.)
- [x] `&` code variable
- [x] `$?FILE`, `$?LINE` compile-time variables
- [x] `$!` (attribute access)
- [x] `$.` (public attribute)
- [x] `$^` placeholder variables

### Operators
- [x] Arithmetic: `+`, `-`, `*`, `/`, `%`, `%%`, `**`, `div`, `mod`
- [x] String: `~`, `x`, `xx`
- [x] Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- [x] String comparison: `eq`, `ne`, `lt`, `le`, `gt`, `ge`
- [x] Logical: `&&`, `||`, `!`, `//`, `and`, `or`, `not`
- [x] Assignment: `=`, `:=`, `+=`, `-=`, `~=`, `*=`
- [x] Increment: `++`, `--` (prefix/postfix)
- [x] Ternary: `?? !!`
- [x] Smartmatch: `~~`
- [x] Range: `..`, `..^`
- [x] Pair: `=>`
- [x] `so` (loose bool coercion)
- [x] `^..`, `^..^` (range variants)
- [x] `<=>`, `leg`, `cmp` (comparison returning Order)
- [x] `eqv` (value equality)
- [x] `===` (identity equality)
- [x] `?` (boolean context prefix)
- [x] `^` (upto: `^10` -> `0..^10`)
- [x] Bitwise: `+&`, `+|`, `+^`, `+<`, `+>`
- [x] Junction operators: `&`, `|`, `^`
- [x] `[op]` reduction meta operator
- [x] Meta operators: `R`, `X`, `Z`
- [x] Meta operators: `op=`
- [x] Hyper operators: `>>op<<`

### Control Flow
- [x] `if` / `elsif` / `else`
- [x] `unless`
- [x] `while`, `until`
- [x] `loop` (C-style)
- [x] `repeat while` / `repeat until`
- [x] `for`
- [x] `for` with pointy block (`-> $x`)
- [x] `given` / `when` / `default`
- [x] `last`, `next`
- [x] `return`
- [x] `die`
- [x] `try` / `CATCH`
- [x] `with` / `without`
- [x] `orwith`
- [x] `proceed`, `succeed`
- [x] `redo`
- [x] Labeled loops
- [x] `CONTROL { }`
- [x] `warn`
- [x] `fail`
- [x] `do { }` block
- [x] `gather` / `take`
- [x] Statement modifiers: `if`, `unless`, `for`, `while`, `until`, `given`, `when`, `with`, `without`

### Subroutines
- [x] `sub` declaration
- [x] Multiple parameters
- [x] Anonymous sub / lambda (`-> $x { }`)
- [x] `return`
- [x] Named parameters
- [x] Default values
- [x] Type constraints (`Int $x`)
- [x] Slurpy parameters (`*@args`, `*%opts`)
- [x] `multi sub`
- [x] `proto sub`
- [x] `MAIN` sub
- [x] Closure (lexical capture)

### Built-in Methods
- [x] `.defined`, `.Bool`, `.Str`, `.Int`, `.Numeric`
- [x] `.elems`, `.chars`, `.uc`, `.lc`
- [x] `.push`, `.pop`, `.shift`, `.unshift`, `.reverse`, `.sort`
- [x] `.keys`, `.values`, `.kv`, `.pairs`, `.exists`
- [x] `.split`, `.join`
- [x] `.WHAT`, `.perl`, `.gist`
- [x] `.map`, `.grep`, `.first`
- [x] `.flat`, `.unique`
- [x] `.squish`
- [x] `.min`, `.max`
- [x] `.minmax`
- [x] `.sum`, `.pick`, `.roll`
- [x] `.comb`, `.contains`, `.starts-with`, `.ends-with`
- [x] `.substr`, `.index`, `.rindex`
- [x] `.chomp`, `.chop`, `.trim`
- [x] `.abs`, `.sqrt`, `.ceiling`, `.floor`, `.round`
- [x] `.base`
- [x] `.parse-base`
- [x] `.Range` (type range)
- [ ] `.new` (constructor)

### Test Module
- [x] `plan`, `done-testing`
- [x] `ok`, `nok`, `is`, `isnt`
- [x] `cmp-ok`, `like`, `unlike`
- [x] `skip`, `skip-rest`, `todo`, `bail-out`
- [x] `subtest`
- [x] `lives-ok`, `dies-ok`
- [x] `eval-lives-ok`
- [x] `throws-like`
- [x] `is-deeply`
- [x] `isa-ok`
- [x] `does-ok`, `can-ok`

### Miscellaneous
- [x] `EVAL`
- [x] `use` (module loading)
- [x] Comments (`#`, embedded comments, POD)
- [x] String interpolation
- [x] Regex (basic)
- [x] Improve `say` formatting (.gist compliant)

---

## Phase 2: Object System

Implement Raku OOP.

- [x] `class` declaration
- [x] `has` attributes (`has $.name`, `has $!private`)
- [x] `method` declaration
- [x] `self`
- [x] `new` constructor (auto-generated)
- [x] Inheritance (`is Parent`)
- [x] `role` declaration and `does`
- [x] `multi method`
- [x] `BUILD` / `TWEAK` submethods
- [x] Type checking
- [x] Coercion (`Int(Str)` etc.)
- [x] `enum`
- [x] `subset`
- [x] Method resolution order (MRO, C3)

---

## Phase 3: Regex and Grammars

Implement Raku regex/grammar.

- [x] Basic regex (`/pattern/`)
- [x] `rx//` form
- [x] `m//` match operator
- [x] `s///` substitution operator
- [x] Character classes, quantifiers, anchors
- [ ] Named captures (`$<name>`)
- [ ] `token`, `rule` declarations
- [ ] `grammar` declaration
- [ ] `proto token` and LTM
- [ ] Action classes
- [ ] `make` / `made`

---

## Phase 4: Advanced Features

- [x] Phasers (`BEGIN`, `END`, `ENTER`, `LEAVE`, `FIRST`, `NEXT`, `LAST`)
- [x] `gather` / `take` (lazy lists)
- [x] Junction (`any`, `all`, `one`, `none`)
- [x] `Promise`, `Supply`, `Channel` (concurrency)
- [x] `react` / `whenever`
- [x] `Proc::Async`
- [ ] `IO::Path` full implementation
- [ ] Module system (`unit module`, `export`, `use`)
- [ ] `MAIN` (command-line argument parsing)
- [ ] `CATCH` type matching (`when X::AdHoc`)
- [ ] `use lib`
- [ ] Precompilation

---

## Phase 5: Performance and Practicality

Aim to exceed MoarVM performance.

### Compiler Infrastructure
- [ ] AST -> bytecode compilation
- [ ] Register-based VM or native code generation
- [ ] Constant folding
- [ ] Inlining
- [ ] Type inference optimization
- [ ] Escape analysis

### Runtime
- [ ] GC (generational or reference counting + cycle detection)
- [ ] Native int/num (avoid boxing)
- [ ] String rope / CoW
- [ ] Hash optimization (small hash optimization)
- [ ] Tail call optimization

### Practicality
- [ ] REPL
- [ ] Debugger
- [ ] Improved error messages (with source location)
- [ ] `zef` package manager compatibility
- [ ] Inline::Perl5 compatibility layer
- [ ] Native binary output

---

## Design Principles

1. **Gradual migration from tree-walking interpreter to bytecode VM**
2. **Learn from MoarVM architecture** while leveraging Rust's strengths
3. **Prioritize startup speed** (a weakness of MoarVM)
4. **Incremental optimization**: make it correct first, then make it fast
