# S03 - Operators

Reference: `old-design-docs/S03-operators.pod`

Covers all operator precedence levels, meta-operators, smart matching, and junctions.

---

## Precedence Levels and Operators

### Term Precedence
- [x] Int/Num/Str literals
- [x] Array composer `[...]`
- [x] Hash composer `{...}`
- [x] Closure `{...}`
- [x] Regex (`/pattern/`, `m//`, `rx//`)
- [x] Subexpressions with parens
- [x] Function call with parens
- [x] Pair composers (`:key(value)`, `key => value`)
- [ ] Capture composer (`\(...)`)
- [ ] Signature literal (`:(...)`)
- [x] Method call with implicit invocant (`.method`)
- [x] Substitution (`s///`)
- [ ] Transliteration (`tr///`)

### Method Postfix Precedence
- [x] Standard method call (`.meth`)
- [ ] Try method call (`.?meth`) - call if exists
- [ ] Call all methods (`.*meth`)
- [ ] Call all, die if none (`.+meth`)
- [ ] Class-qualified method (`.::Class::meth`)
- [x] Mutating method call (`.=meth`)
- [ ] Meta-method call (`.^meth` for HOW)
- [x] Postcircumfix `.[]`, `.{}`, `.<>`
- [ ] Postcircumfix `.()`, `.<<>>`
- [ ] Dotted postfix for any postfix (`.++`)
- [x] Imaginary number postfix (`42i`)

### Autoincrement Precedence
- [x] `prefix:<++>` and `postfix:<++>`
- [x] `prefix:<-->` and `postfix:<-->`
- [ ] String increment/decrement with Unicode range support
- [ ] Boolean increment/decrement

### Exponentiation Precedence
- [x] `infix:<**>` exponentiation

### Symbolic Unary Precedence
- [x] `prefix:<?>` boolean context
- [x] `prefix:<!>` boolean negation
- [x] `prefix:<+>` numeric context
- [x] `prefix:<->` numeric negation
- [x] `prefix:<~>` string context
- [ ] `prefix:<|>` flatten object into arglist
- [ ] `prefix:<||>` flatten into semicolon list
- [x] `prefix:<+^>` numeric bitwise negation
- [ ] `prefix:<~^>` string bitwise negation
- [ ] `prefix:<?^>` boolean negation (same as `!`)
- [x] `prefix:<^>` upto operator (`0..^$limit`)

### Multiplicative Precedence
- [x] `infix:<*>` multiplication
- [x] `infix:</>` division
- [x] `infix:<div>` integer division
- [x] `infix:<%>` modulo
- [x] `infix:<%%>` is divisible by
- [x] `infix:<mod>` integer modulo
- [x] `infix:<+&>` numeric bitwise and
- [x] `infix:<+<>` numeric shift left
- [x] `infix:<+>>` numeric shift right
- [ ] `infix:<~&>` buffer bitwise and
- [ ] `infix:<~<>` buffer shift left
- [ ] `infix:<~>>` buffer shift right
- [ ] `infix:<?&>` boolean and
- [ ] `infix:<gcd>` greatest common divisor
- [ ] `infix:<lcm>` least common multiple

### Additive Precedence
- [x] `infix:<+>` numeric addition
- [x] `infix:<->` numeric subtraction
- [x] `infix:<+|>` numeric bitwise or
- [x] `infix:<+^>` numeric bitwise xor
- [ ] `infix:<~|>` buffer bitwise or
- [ ] `infix:<~^>` buffer bitwise xor
- [ ] `infix:<?|>` boolean or
- [ ] `infix:<?^>` boolean xor

### Replication Precedence
- [x] `infix:<x>` string replication
- [x] `infix:<xx>` list repetition

### Concatenation Precedence
- [x] `infix:<~>` string concatenation

### Junctive Precedence
- [x] `infix:<&>` all junction (junctive and)
- [x] `infix:<|>` any junction (junctive or)
- [x] `infix:<^>` one junction (junctive xor)

### Named Unary Precedence
- [ ] `let` operator (hypothetical values)
- [ ] `temp` operator (temporary values)

### Nonchaining Binary Precedence
- [ ] `infix:<but>` mixins
- [ ] `infix:<does>` role composition at runtime
- [x] `infix:<<=>>`  numeric three-way comparison
- [x] `infix:<leg>` string three-way comparison
- [x] `infix:<cmp>` generic three-way comparison
- [x] Range constructors (`..`, `^..`, `..^`, `^..^`)

### Chaining Binary Precedence
- [x] `infix:<==>` numeric equality
- [x] `infix:<!=>` numeric inequality
- [x] `infix:<eq>`, `infix:<ne>` string equality
- [x] `infix:<lt>`, `infix:<le>`, `infix:<gt>`, `infix:<ge>` string ordering
- [x] `infix:<~~>` smart match
- [x] `infix:<===>` value identity
- [x] `infix:<eqv>` canonical equivalence
- [ ] `infix:<before>`, `infix:<after>` generic ordering
- [ ] Negated operators (`!==`, `!eq`, `!~~`, etc.)
- [ ] Chained comparisons (`1 < $x < 10`)

### Tight And / Or Precedence
- [x] `infix:<&&>` short-circuit and
- [x] `infix:<||>` short-circuit or
- [ ] `infix:<^^>` short-circuit exclusive or
- [x] `infix:<//>` defined-or
- [ ] `infix:<min>`, `infix:<max>` (as operators)

### Conditional Operator Precedence
- [x] Ternary `?? !!`
- [ ] Flipflop `ff`, `fff` with `^` variants

### Item Assignment Precedence
- [x] `infix:<=>` assignment
- [x] `infix:<=>>` fat arrow (Pair constructor)
- [x] Assignment operators (`+=`, `-=`, `*=`, `~=`, `.=`)
- [ ] All `op=` variants for any infix

### Loose Unary Precedence
- [x] `prefix:<not>` boolean negation
- [x] `prefix:<so>` boolean truth

### Comma Operator Precedence
- [x] `infix:<,>` argument separator
- [ ] `infix:<:>` invocant marker

### List Infix Precedence
- [x] `infix:<Z>` zip operator
- [x] `infix:<X>` cross operator
- [ ] `infix:<minmax>` minmax operator
- [ ] `infix:<...>` sequence operator
- [ ] `infix:<...^>` sequence operator excluding endpoint

### List Prefix Precedence
- [x] List assignment `=`
- [x] Binding `:=`
- [ ] Read-only binding `::=`
- [x] Normal listops (print, push, say, join, etc.)
- [x] Junctional operators (any, all, one, none)
- [x] `fail`, `die`, `warn`
- [ ] Stubby exception generators (`...`, `!!!`, `???`)
- [x] Reduce operators (`[+]`, `[*]`, etc.)

### Loose And / Or Precedence
- [x] `infix:<and>` short-circuit and
- [x] `infix:<or>` short-circuit or
- [ ] `infix:<xor>` exclusive or
- [ ] `infix:<andthen>` proceed on success
- [ ] `infix:<orelse>` proceed on failure
- [ ] `infix:<notandthen>`

### Terminator Precedence
- [ ] Feed operators (`==>`, `<==`, `==>>`, `<<==>`)
- [x] Statement modifiers (if, unless, while, for, etc.)

---

## Meta-operators

### Assignment Meta-operator (`op=`)
- [x] `+=`, `-=`, `*=`, `~=`, `.=`
- [ ] Generic `op=` for any infix operator
- [ ] Identity value auto-vivification for undefined containers

### Negated Relational Operators
- [ ] `!` prefix on any Bool-returning infix (`!==`, `!eq`, `!~~`)

### Reversed Operators (`R`)
- [x] `R` prefix to reverse arguments (`Rcmp`, etc.)

### Hyper Operators
- [x] Binary hypers: `>>op<<`, `>>op>>`, `<<op<<`, `<<op>>`
- [ ] Unary hypers: `>>method`, `<<method`
- [ ] Hash hypers with key intersection/union
- [ ] Recursive hyper application to nested structures
- [ ] DWIMmy shape matching semantics

### Reduction Operators
- [x] `[op]` form for list reduction
- [ ] `[\op]` triangular reduction (intermediate results)
- [ ] Identity values for zero-argument reduction
- [ ] Chaining reduction handling

### Cross Operators
- [x] `Xop` cross product application
- [ ] Full cross meta-operator nesting

### Zip Operators
- [x] `Zop` zip with operator
- [ ] Termination semantics (shortest list)

### Sequential Operators
- [ ] `Sop` meta-operator (suppress parallelism)

### Other Meta-features
- [ ] `&[+]` turning infix into noun
- [ ] `[&function]` turning function into infix
- [ ] Nesting of meta-operators

---

## Smart Matching

- [x] Basic smart match (`~~`)
- [ ] Comprehensive type-based dispatch table
- [ ] `.ACCEPTS` method interface
- [ ] Smart matching against: callables, sets/bags, regexes, ranges, types, signatures
- [x] Smart matching against literals and types (partial)

---

## Junctions

- [x] Junction constructors (`|`, `&`, `^`, any, all, one, none)
- [x] Junction autothreading
- [ ] Sequential junction variants (`S&`, `S|`, `S^`)

---

## Comparison Semantics

- [x] `===` value identity
- [x] `eqv` canonical equivalence
- [x] `cmp` generic three-way comparison (returning Order)
- [x] `leg` string three-way comparison
- [x] `<=>` numeric three-way comparison
- [ ] `before`, `after` generic ordering

---

## Declarators

- [x] `my`, `has`
- [ ] `our`, `state` (partial)
- [x] `constant`
- [ ] Type declarators: `package`, `module`, `class`, `role`, `subset`, `enum` (partial)
- [ ] Code declarators: `sub`, `method`, `submethod`, `multi`, `proto`, `macro` (partial)
