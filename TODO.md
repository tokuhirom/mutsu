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
- [x] Named captures (`$<name>`)
- [x] `token`, `rule` declarations
- [x] `grammar` declaration
- [x] `proto token` and LTM
- [ ] Action classes
- [x] `make` / `made`

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

### Bytecode VM — Compilation Progress

Hybrid stack-based VM with fallback to tree-walker (`InterpretExpr`/`InterpretStmt`).

**Phase 1–2 (done):** Literals, `$` variables, arithmetic, comparison (`==`/`!=`/`<`/`>`/`eq`/`ne`), string concat/interpolation, short-circuit operators (`&&`/`||`/`//`/`andthen`/`orelse`), control flow (`if`/`else`, ternary), loops (`while`/`for`/C-style `loop`) with `last`/`next`/`redo`/labeled loops, `say`/`print`, `return`.

**Phase 3 (done):** Function calls (`CallFunc`/`ExecCall`), method calls on non-variable targets (`CallMethod`), array/hash indexing (`Index`), string interpolation (`StringConcat`), postfix `++`/`--`, hash literals (`MakeHash`), `AssignExpr`, C-style loop (`CStyleLoop`).

**Phase 4 (done):** `@`/`%` variable access (`GetArrayVar`/`GetHashVar`), bareword resolution (`GetBareWord`), range ops (`MakeRange`/`MakeRangeExcl`/`MakeRangeExclStart`/`MakeRangeExclBoth`), string comparisons (`StrLt`/`StrGt`/`StrLe`/`StrGe`), method calls on variable targets with writeback (`CallMethodMut`), `die`.

**Phase 5 (done):** Unary ops (`+`/`~`/`^`/`so`/prefix `++`/`--`), `CaptureVar`/`CodeVar`, `given`/`when`/`default`, `repeat while`/`repeat until`, `:=` bind assignment, prefix `+` Bool fix.

**Phase 6 (done):** All remaining binary ops (`~~`/`!~~`/`<=>`/`cmp`/`leg`/`===`/`eqv`/`%%`/`div`/`mod`/`gcd`/`lcm`/`x`/`xx`/`=>`/bitwise/set ops/`...`), `proceed`/`succeed`, match-assign (`=~`).

**Phase 7 (done):** Expression compilation: `EnvIndex` (%*ENV<key>), `Exists` (:exists), `Reduction` ([+] @arr), `Subst` (s///), `RoutineMagic`/`BlockMagic`.

**Phase 8 (done):** All remaining statements compiled. No-ops (`Catch`/`Control`/`HasDecl`/`MethodDecl`/`DoesDecl`) emit nothing. `Take` compiled to native opcode. `React`/`Package` body compiled inline. `Phaser` (BEGIN inline, END deferred). Declarations (`SubDecl`/`ClassDecl`/`RoleDecl`/`EnumDecl`/`SubsetDecl`/`TokenDecl`/`RuleDecl`/`ProtoDecl`/`ProtoToken`/`Use`/`Subtest`/`Whenever`) and `Call` with named/Block args delegate to interpreter.

#### Compiled Binary Ops
- [x] Arithmetic: `+`, `-`, `*`, `/`, `%`, `**`
- [x] String: `~` (concat)
- [x] Numeric comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- [x] String comparison: `eq`, `ne`, `lt`, `gt`, `le`, `ge`
- [x] Range: `..`, `..^`, `^..`, `^..^`
- [x] Smart match: `~~`, `!~~`
- [x] Three-way: `<=>`, `cmp`, `leg`
- [x] Identity/value: `===`, `eqv`
- [x] Divisibility: `%%`
- [x] Keyword math: `div`, `mod`, `gcd`, `lcm`
- [x] Repetition: `x`, `xx`
- [x] Pair: `=>` (FatArrow)
- [x] Bitwise: `+&`, `+|`, `+^`, `+<`, `+>`
- [x] Set ops: `(elem)`, `(cont)`, `(|)`, `(&)`, `(-)`, `(^)`, `(<=)`, `(>=)`, `(<)`, `(>)`
- [x] Sequence: `...`

#### Compiled Unary Ops
- [x] `-` (negate), `!` (not), `?` (bool), `+` (numeric), `~` (string), `^` (upto)
- [x] `so` (bool coerce), prefix `++`/`--`

#### Compiled Expressions
- [x] `Literal`, `Var`, `ArrayVar`, `HashVar`, `BareWord`
- [x] `Binary` (with compiled opcodes — others fall back)
- [x] `Unary` (most ops — rest fall back)
- [x] `Ternary`, `ArrayLiteral`, `Hash`
- [x] `Call`, `MethodCall` (both variable and non-variable targets)
- [x] `Index`, `StringInterpolation`
- [x] `PostfixOp` (++/-- on Var), `AssignExpr`
- [x] `CaptureVar`, `CodeVar`
- [x] `EnvIndex` (%*ENV<key>)
- [x] `Subst` (s///)
- [x] `Exists` (:exists)
- [x] `Reduction` ([+] @arr)
- [x] `RoutineMagic` / `BlockMagic`
- [ ] `Block` / `AnonSub` (as expression value)
- [ ] `Lambda` (-> $x { })
- [ ] `CallOn` (target.())
- [ ] `Try` (try { } CATCH { })
- [ ] `Gather` (gather { take … })
- [ ] `InfixFunc` (min, max infix)
- [ ] `HyperOp` (>>op<<)
- [ ] `MetaOp` (Rop, Xop, Zop)

#### Compiled Statements
- [x] `Expr`, `Block`, `Say`, `Print`, `VarDecl`
- [x] `Assign` (=, :=, =~), `If`, `While`, `For`, `Loop` (C-style, repeat)
- [x] `Call` (positional args only), `Last`, `Next`, `Redo`, `Return`, `Die`
- [x] `Given`, `When`, `Default`, `Proceed`, `Succeed`
- [x] `Call` with named args / Block / AnonSub args (delegate to interpreter)
- [x] `SubDecl` / `ProtoDecl` (delegate to interpreter)
- [x] `ClassDecl` / `RoleDecl` (delegate to interpreter)
- [x] `HasDecl` / `MethodDecl` / `DoesDecl` (no-op outside class)
- [x] `EnumDecl` / `SubsetDecl` (delegate to interpreter)
- [x] `TokenDecl` / `RuleDecl` / `ProtoToken` (delegate to interpreter)
- [x] `Use` / `Package` (Use delegates; Package compiled inline)
- [x] `Phaser` (BEGIN inline, END deferred, others delegate)
- [x] `Catch` / `Control` (no-op, handled by try)
- [x] `Take` (compiled to Take opcode)
- [x] `React` (compiled inline) / `Whenever` (delegate to interpreter)
- [x] `Subtest` (delegate to interpreter)

#### Remaining: VM Architecture
- [ ] Local variable slots (`GetLocal`/`SetLocal` — indexed, no HashMap lookup)
- [ ] Scope management (push/pop env frames in VM)
- [ ] Functions/closures in VM (`MakeClosure`, upvalues)
- [ ] Native method dispatch in VM (bypass interpreter bridge)
- [ ] Remove fallback opcodes (full native compilation)

### Optimization Pipeline
- [ ] Constant folding
- [ ] Dead code elimination
- [ ] Inlining (small subs)
- [ ] Type inference (speculative optimization)
- [ ] Escape analysis
- [ ] Register-based VM or native code generation

### Runtime / Memory
- [ ] GC or memory management strategy (see notes below)
- [ ] Native int/num (avoid boxing for hot paths)
- [ ] String rope / CoW (reduce clone overhead)
- [ ] Small hash optimization (inline ≤4 entries)
- [ ] Tail call optimization

#### GC Status and Plan
Currently Rust ownership handles deallocation. All `Value` instances are owned
(`String`, `Vec`, `HashMap`) and freed when dropped. `Rc<LazyList>` is the only
reference-counted type. `clone()` is used extensively to pass values across env
boundaries.

**Current issues:**
- Excessive cloning: every variable read/write clones the `Value`, including
  deep structures like `Array(Vec<Value>)` and `Hash(HashMap<…>)`.
- Env grows monotonically: variables are never removed from the flat `HashMap`.
- No cycle detection needed yet (`Value` types cannot form cycles), but user-defined
  objects with mutual references could in the future.

**Recommended roadmap:**
1. **Short-term:** Introduce `Rc<Value>` (or `Rc<RefCell<Value>>`) for large values
   to replace deep clones with reference bumps. Measure impact.
2. **Medium-term:** Implement scoped environments (stack of frames) so local
   variables are naturally freed on scope exit. Pairs with `GetLocal`/`SetLocal`.
3. **Long-term:** If cycles become possible (e.g. closures capturing themselves,
   circular object graphs), add either tracing GC or weak-ref cycle collector on
   top of Rc.

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
