# VM Compilation History

Detailed log of the bytecode VM compilation progress (Phase 1-16).
Moved from TODO.md to keep the roadmap focused on actionable items.

## Architecture

Hybrid stack-based VM with fallback to tree-walker (`InterpretExpr`/`InterpretStmt`).

## Phase 1-2 (done)

Literals, `$` variables, arithmetic, comparison (`==`/`!=`/`<`/`>`/`eq`/`ne`), string concat/interpolation, short-circuit operators (`&&`/`||`/`//`/`andthen`/`orelse`), control flow (`if`/`else`, ternary), loops (`while`/`for`/C-style `loop`) with `last`/`next`/`redo`/labeled loops, `say`/`print`, `return`.

## Phase 3 (done)

Function calls (`CallFunc`/`ExecCall`), method calls on non-variable targets (`CallMethod`), array/hash indexing (`Index`), string interpolation (`StringConcat`), postfix `++`/`--`, hash literals (`MakeHash`), `AssignExpr`, C-style loop (`CStyleLoop`).

## Phase 4 (done)

`@`/`%` variable access (`GetArrayVar`/`GetHashVar`), bareword resolution (`GetBareWord`), range ops, string comparisons, method calls on variable targets with writeback (`CallMethodMut`), `die`.

## Phase 5 (done)

Unary ops (`+`/`~`/`^`/`so`/prefix `++`/`--`), `CaptureVar`/`CodeVar`, `given`/`when`/`default`, `repeat while`/`repeat until`, `:=` bind assignment.

## Phase 6 (done)

All remaining binary ops (~50), `proceed`/`succeed`, match-assign.

## Phase 7 (done)

Expression compilation: `EnvIndex`, `Exists`, `Reduction`, `Subst`, `RoutineMagic`/`BlockMagic`.

## Phase 8 (done)

All remaining statements compiled. Declarations delegate to interpreter.

## Phase 9 (done)

All remaining expressions compiled. Bridge opcodes for complex sub-expressions.

## Phase 10 (done)

Local variable slots (`GetLocal`/`SetLocal`), native zero-arg method dispatch (12 methods).

## Phase 11 (done)

Compiled function bodies (`CompiledFunction`), `TryCatch` opcode, block inlining, 17 additional zero-arg methods.

## Phase 12 (done)

Native binary operations — all ~50 binary ops execute directly in VM. Junction auto-threading.

## Phase 13 (done)

Eliminate interpreter dependencies — static associated functions, inlined Sequence/Index, removed bridge wrappers.

## Phase 14 (done)

Native one-arg method dispatch (10 methods), 7 new zero-arg methods.

## Phase 15 (done)

Native function dispatch (~30 functions), two-arg methods, bridge elimination.

## Phase 16 (done)

3-arg and variadic function dispatch, 8 zero-arg methods, 2 one-arg methods.

## Current native dispatch coverage

### Native methods
- Zero-arg (38): `.defined`, `.Bool`, `.Str`, `.Int`, `.Numeric`, `.Num`, `.chars`, `.elems`, `.abs`, `.uc`, `.lc`, `.sign`, `.end`, `.flat`, `.sort`, `.reverse`, `.unique`, `.keys`, `.values`, `.floor`, `.ceiling`, `.round`, `.sqrt`, `.words`, `.lines`, `.trim`, `.trim-leading`, `.trim-trailing`, `.so`, `.not`, `.chomp`, `.chop`, `.comb`, `.gist`, `.raku`, `.perl`, `.head`, `.tail`, `.first`, `.tclc`, `.succ`, `.pred`, `.log`, `.exp`, `.min`, `.max`, `.Rat`
- One-arg (15): `.contains`, `.starts-with`, `.ends-with`, `.index`, `.substr`, `.split`, `.join`, `.head(n)`, `.tail(n)`, `.base`, `.rindex`, `.fmt`, `.parse-base`, `.round(scale)`, `.log(base)`
- Two-arg (1): `.substr(start, len)`

### Native functions
- 1-arg (34): `abs`, `sqrt`, `floor`, `ceiling`/`ceil`, `round`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `truncate`, `chr`, `ord`, `uc`, `lc`, `tc`, `chomp`, `chop`, `trim`, `flip`, `words`, `chars`, `defined`, `elems`, `reverse`, `sort`, `flat`, `first`, `min`, `max`, `ords`, `gist`
- 2-arg (8): `join`, `index`, `substr`, `atan2`, `log`, `round`, `min`, `max`
- 3-arg (1): `substr`
- Variadic (4): `min`, `max`, `chrs`, `flat`

### Remaining (not yet native)
- Methods (zero-arg): `.Complex`, `.sin`, `.cos`, `.tan`
- Methods (one-arg): `.comb(pattern-str)`, `.substr-rw`
- Methods (two-arg+): `.split(sep,limit)`, `.index(needle,pos)`, `.rindex(needle,pos)`, `.base(radix,digits)`

## Compiled ops summary

All binary ops, unary ops, expressions, and statements are compiled.
Closures (Lambda/AnonSub/Gather) and class/role declarations still delegate to interpreter.
