# TODO - Raku Language Features

Based on design docs:
- `/home/tokuhirom/work/old-design-docs/S02-bits.pod`
- `/home/tokuhirom/work/old-design-docs/S03-operators.pod`
- `/home/tokuhirom/work/old-design-docs/S04-control.pod`

## Legend
- [x] Implemented
- [ ] Not implemented
- [~] Partially implemented

---

## S02: Lexical Conventions

### Comments
- [x] Single-line comments (`#`)
- [x] Embedded comments (`#`(...)`, `#`[...]`, etc.)
- [ ] Multiline POD comments (`=begin comment` ... `=end comment`)
- [ ] Single paragraph POD comments (`=comment`)

### Brackets and Whitespace
- [x] Basic bracketing characters (`()`, `{}`, `[]`, `<>`)
- [x] Unicode brackets for embedded comments
- [ ] User-selected brackets (multiple identical brackets like `<<<...>>>`)
- [ ] Unspaces (`\` to continue across whitespace)

### Built-In Data Types

#### Numeric Types
- [x] Int
- [x] FatRat
- [ ] Rat (rational numbers)
- [ ] Num (floating point)
- [ ] Complex

#### Other Types
- [x] Str
- [x] Bool
- [x] Array
- [x] Range (`..`, `..^`)
- [ ] Hash
- [ ] Set, Bag, Mix
- [~] Nil (partial)

### Literals

#### Numeric Literals
- [x] Integer literals
- [ ] Underscores in numbers (`1_000_000`)
- [ ] Radix markers (`0x`, `0o`, `0b`)
- [ ] General radices (`:16<DEAD_BEEF>`)
- [ ] Exponentials (`1e10`)
- [ ] Rational literals (`1/3`)
- [ ] Complex literals (`1+2i`)

#### String Literals
- [x] Double-quoted strings with interpolation
- [x] Single-quoted strings
- [ ] Q forms (`Q`, `q`, `qq`)
- [ ] Heredocs (`q:to/END/`)
- [ ] Angle quotes (`<word list>`)

### Variables and Names

#### Sigils
- [x] `$` (scalar)
- [x] `@` (array)
- [ ] `%` (hash)
- [ ] `&` (code)

#### Twigils
- [x] `*` (dynamic/global: `$*PID`, `$*USER`, etc.)
- [ ] `?` (compile-time: `$?FILE`, `$?LINE`)
- [ ] `!` (attribute)
- [ ] `.` (public attribute)
- [ ] `^` (placeholder/positional)
- [ ] `:` (placeholder/named)

---

## S03: Operators (by precedence, tightest to loosest)

### Terms
- [x] Int/Num literals
- [x] String literals
- [x] Variables (`$x`, `@y`)
- [x] Array composer (`[1,2,3]`)
- [ ] Hash composer (`{ a => 1 }`)
- [ ] Closure (`{ ... }`)
- [ ] Capture composer (`\(...)`)
- [ ] Pair composers (`:key(value)`, `:!verbose`)

### Method Postfix (.meth)
- [~] Standard method calls (`$obj.meth`)
- [ ] Method variants (`.+meth`, `.?meth`, `.*meth`)
- [ ] Class-qualified (`$obj.::Class::meth`)
- [ ] Mutating method (`.=meth`)
- [ ] Meta-method (`.^meth`)
- [x] Subscripts (`.[]`, `.{}`)

### Autoincrement
- [x] `++` (prefix and postfix)
- [x] `--` (prefix and postfix)

### Exponentiation
- [x] `**`

### Symbolic Unary
- [x] `!` (boolean negation)
- [x] `+` (numeric context)
- [x] `-` (numeric negation)
- [x] `~` (string context)
- [ ] `?` (boolean context)
- [ ] `|` (flatten into arglist)
- [ ] `||` (flatten into semicolon list)
- [ ] `+^` (numeric bitwise negation)
- [ ] `~^` (string bitwise negation)
- [ ] `?^` (boolean negation, same as `!`)
- [ ] `^` (upto operator: `^$n` means `0..^$n`)

### Multiplicative
- [x] `*`
- [x] `/`
- [x] `%` (modulo)
- [ ] `%%` (divisible by)
- [ ] `div` (integer division)
- [ ] `mod` (integer modulo)
- [ ] `+&` (numeric bitwise AND)
- [ ] `+<` (numeric shift left)
- [ ] `+>` (numeric shift right)
- [ ] `~&`, `~<`, `~>` (buffer bitwise ops)
- [ ] `?&` (boolean AND)
- [ ] `gcd` (greatest common divisor)
- [ ] `lcm` (least common multiple)

### Additive
- [x] `+`
- [x] `-`
- [ ] `+|` (numeric bitwise OR)
- [ ] `+^` (numeric bitwise XOR)
- [ ] `~|`, `~^` (buffer bitwise ops)
- [ ] `?|` (boolean OR)
- [ ] `?^` (boolean XOR)

### Replication
- [ ] `x` (string replication)
- [ ] `xx` (list replication)

### Concatenation
- [x] `~` (string concatenation)

### Junctive AND
- [ ] `&` (all junction)
- [ ] `(&)`, `∩` (set intersection)

### Junctive OR
- [ ] `|` (any junction)
- [ ] `^` (one junction)
- [ ] `(|)`, `∪` (set union)

### Named Unary
- [ ] `temp`
- [ ] `let`

### Structural Infix (non-chaining)
- [ ] `but` (mixin)
- [ ] `does` (mixin)
- [ ] `<=>` (numeric comparison, returns Order)
- [ ] `leg` (string comparison, returns Order)
- [ ] `cmp` (generic comparison, returns Order)
- [x] `..` (range, inclusive)
- [x] `..^` (range, excluding end)
- [ ] `^..` (range, excluding start)
- [ ] `^..^` (range, excluding both)

### Chaining Infix (comparison)
- [x] `==`, `!=`
- [x] `<`, `<=`, `>`, `>=`
- [x] `eq`, `ne`
- [x] `lt`, `le`, `gt`, `ge`
- [x] `~~` (smartmatch)
- [ ] `!~~` (negated smartmatch)
- [ ] `===` (value identity)
- [ ] `eqv` (canonical equivalence)
- [ ] `=:=` (container identity)
- [ ] `before`, `after` (generic ordering)

### Tight AND
- [x] `&&`

### Tight OR
- [x] `||`
- [ ] `^^` (exclusive or)
- [x] `//` (defined-or)
- [ ] `min`, `max`

### Conditional
- [ ] `?? !!` (ternary)
- [ ] `ff`, `fff` (flipflop)

### Item Assignment
- [x] `=`
- [x] `=>` (Pair constructor)
- [~] Assignment operators (`+=`, `-=`, `~=`, etc.)
- [ ] `.=` (mutating method)

### Loose Unary
- [x] `not`
- [ ] `so`

### Comma
- [x] `,` (list separator)
- [ ] `:` (invocant marker)

### List Infix
- [ ] `Z` (zip)
- [ ] `X` (cross)
- [ ] `...` (sequence)
- [ ] `minmax`

### List Prefix
- [x] `print`, `say`
- [ ] `push`, `pop`, `shift`, `unshift`
- [ ] `map`, `grep`, `sort`
- [ ] `join`, `split`
- [ ] `die`, `warn`, `fail`
- [ ] Reduce operators (`[+]`, `[*]`, etc.)
- [ ] `any`, `all`, `one`, `none` (junctions)

### Loose AND
- [x] `and`
- [ ] `andthen`
- [ ] `notandthen`

### Loose OR
- [x] `or`
- [ ] `xor`
- [ ] `orelse`

### Metaoperators
- [ ] Negation (`!op`: `!eq`, `!~~`)
- [ ] Assignment (`op=`: `+=`, `~=`)
- [ ] Hyper (`>>op<<`, `»op«`)
- [ ] Reduce (`[op]`: `[+]`, `[*]`)
- [ ] Cross (`Xop`: `X~`, `X*`)
- [ ] Zip (`Zop`: `Z~`, `Z*`)
- [ ] Sequence (`S` for sequential evaluation)

---

## S04: Control Flow

### Conditional Statements
- [x] `if` / `elsif` / `else`
- [x] `unless`
- [ ] `with` / `orwith` / `else` (definedness test)
- [ ] `without`
- [ ] Parameter binding (`if expr -> $x { }`)

### Loop Statements
- [x] `while`
- [x] `until`
- [ ] `repeat while` / `repeat until`
- [x] `loop` (C-style for)
- [x] `for`
- [ ] `for` with multiple parameters (`for %h.kv -> $k, $v`)
- [ ] `for` with `Z` (zip)
- [ ] Lazy iteration

### Loop Control
- [x] `next`
- [x] `last`
- [ ] `redo`
- [ ] Labeled loops (`LABEL: for ...`)
- [ ] `LABEL.next`, `LABEL.last`

### Switch Statements
- [ ] `given` / `when` / `default`
- [ ] `proceed` (continue to next when)
- [ ] `succeed` (break with value)

### Exception Handling
- [ ] `try { }`
- [ ] `CATCH { }`
- [ ] `CONTROL { }`
- [ ] `die`, `warn`, `fail`
- [ ] Resumable exceptions
- [ ] `use fatal`

### Phasers
- [ ] `BEGIN` (compile time, ASAP)
- [ ] `CHECK` (compile time, ALAP)
- [ ] `INIT` (run time, ASAP)
- [ ] `END` (run time, ALAP)
- [ ] `ENTER` (block entry)
- [ ] `LEAVE` (block exit)
- [ ] `KEEP` (successful exit)
- [ ] `UNDO` (unsuccessful exit)
- [ ] `FIRST` (loop init)
- [ ] `NEXT` (loop continuation)
- [ ] `LAST` (loop termination)
- [ ] `PRE` / `POST` (assertions)

### Statement Prefixes
- [ ] `do { }`
- [ ] `gather` / `take`
- [ ] `lazy`
- [ ] `eager`
- [ ] `sink`
- [ ] `quietly`
- [ ] `once`
- [ ] `start` (promises)

### Other Control
- [ ] `return`
- [ ] `leave`
- [ ] `goto`

---

## Testing (Test module)

- [x] `plan`
- [x] `ok`, `nok`
- [x] `is`, `isnt`
- [x] `cmp-ok`
- [x] `like`, `unlike`
- [x] `done-testing`
- [x] `skip`, `skip-rest`
- [x] `todo`
- [x] `bail-out`
- [ ] `subtest`
- [ ] `throws-like`
- [ ] `lives-ok`, `dies-ok`
- [ ] `eval-lives-ok`, `eval-dies-ok`
- [ ] `is-deeply`
- [ ] `isa-ok`
- [ ] `does-ok`
- [ ] `can-ok`

---

## Priority for Roast Compatibility

### High Priority
1. Hash support (`%`)
2. Ternary operator (`?? !!`)
3. `given` / `when` / `default`
4. `try` / `CATCH`
5. `throws-like`, `lives-ok`, `dies-ok`
6. `EVAL`
7. Method chaining improvements
8. `subtest`

### Medium Priority
1. Junctions (`any`, `all`, `one`, `none`)
2. `gather` / `take`
3. Reduce operators (`[+]`, `[*]`)
4. `x` and `xx` replication
5. Metaoperators

### Lower Priority
1. Phasers (BEGIN, END, etc.)
2. Unicode operators
3. Set/Bag/Mix types
4. Complex numbers
5. Bitwise operators
