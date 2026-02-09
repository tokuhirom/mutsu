# mutsu - Roadmap

Goal: Build a practical Raku (Perl 6) runtime in Rust that is faster than MoarVM.

KPI: Pass count of `tools/run_all_roast.sh --save` (currently 257/1427)

Development policy: In Phases 1-2, prioritize feature implementation without running roast. From Phase 3 onward, run roast at each milestone to measure progress.

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
- [ ] Rat (rational number)
- [ ] Complex
- [ ] Set, Bag, Mix
- [ ] Enum
- [ ] Junction

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
- [ ] Regex literals

### Variables
- [x] `$` scalar
- [x] `@` array
- [x] `%` hash
- [x] `$_` topic variable
- [x] `$!` error variable
- [x] `$*` dynamic variables (`$*PID`, `$*CWD`, etc.)
- [ ] `&` code variable
- [x] `$?FILE`, `$?LINE` compile-time variables
- [ ] `$!` (attribute access)
- [ ] `$.` (public attribute)
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
- [ ] Junction operators: `&`, `|`, `^`
- [x] `[op]` reduction meta operator
- [ ] Meta operators: `R`, `X`, `Z`, `op=`
- [ ] Hyper operators: `>>op<<`

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
- [ ] `CONTROL { }`
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
- [ ] Type constraints (`Int $x`)
- [x] Slurpy parameters (`*@args`, `*%opts`)
- [x] `multi sub`
- [ ] `proto sub`
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
- [ ] `.Range` (type range)
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
- [ ] Regex (basic)
- [ ] Improve `say` formatting (.gist compliant)

---

## Phase 2: Object System

Implement Raku OOP. Pass roast S12 tests.

- [ ] `class` declaration
- [ ] `has` attributes (`has $.name`, `has $!private`)
- [ ] `method` declaration
- [ ] `self`
- [ ] `new` constructor (auto-generated)
- [ ] Inheritance (`is Parent`)
- [ ] `role` declaration and `does`
- [ ] `multi method`
- [ ] `BUILD` / `TWEAK` submethods
- [ ] Type checking
- [ ] Coercion (`Int(Str)` etc.)
- [ ] `enum`
- [ ] `subset`
- [ ] Method resolution order (MRO, C3)

---

## Phase 3: Regex and Grammars

Implement Raku regex/grammar. Pass roast S05 tests.

- [ ] Basic regex (`/pattern/`)
- [ ] `rx//` form
- [ ] `m//` match operator
- [ ] `s///` substitution operator
- [ ] Character classes, quantifiers, anchors
- [ ] Named captures (`$<name>`)
- [ ] `token`, `rule` declarations
- [ ] `grammar` declaration
- [ ] `proto token` and LTM
- [ ] Action classes
- [ ] `make` / `made`

---

## Phase 4: Advanced Features

- [ ] Phasers (`BEGIN`, `END`, `ENTER`, `LEAVE`, `FIRST`, `NEXT`, `LAST`)
- [ ] `gather` / `take` (lazy lists)
- [ ] Junction (`any`, `all`, `one`, `none`)
- [ ] `Promise`, `Supply`, `Channel` (concurrency)
- [ ] `react` / `whenever`
- [ ] `Proc::Async`
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
2. **Use roast compatibility as a positive metric** to measure spec compliance
3. **Learn from MoarVM architecture** while leveraging Rust's strengths
4. **Prioritize startup speed** (a weakness of MoarVM)
5. **Incremental optimization**: make it correct first, then make it fast
