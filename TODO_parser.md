# TODO: Parser — Missing Operators & Grammar

## Infix Operators

- [x] `xx` — list repetition (Replication level)
- [ ] `o` / `∘` — function composition (Concatenation level)
- [x] `min` — infix min (Tight OR level)
- [x] `max` — infix max (Tight OR level)
- [ ] `ff` — flipflop (Conditional level)
- [ ] `^ff` — flipflop excluding start
- [ ] `ff^` — flipflop excluding end
- [ ] `^ff^` — flipflop excluding both
- [ ] `fff` — sed-style flipflop (Conditional level)
- [ ] `^fff` — sed-style flipflop excluding start
- [ ] `fff^` — sed-style flipflop excluding end
- [ ] `^fff^` — sed-style flipflop excluding both
- [x] `andthen` — defined-and with $_ binding (Loose AND level)
- [x] `notandthen` — undefined-and with $_ binding (Loose AND level)
- [x] `orelse` — defined-or with $_ binding (Loose OR level)
- [ ] `==>` — rightward feed operator (Sequencer level)
- [ ] `<==` — leftward feed operator (Sequencer level)
- [ ] `minmax` — range from min to max (List Infix level)
- [ ] `=~=` / `≅` — approximately equal (Chaining level)
- [ ] `=:=` — container identity (Chaining level)
- [ ] `unicmp` — Unicode comparison (Structural level)
- [ ] `coll` — collation-aware comparison (Structural level)
- [ ] `~&` — string buffer bitwise AND (Multiplicative level)
- [ ] `~|` — string buffer bitwise OR (Additive level)
- [ ] `~^` — string buffer bitwise XOR (Additive level)
- [ ] `~<` — string buffer left shift (Multiplicative level)
- [ ] `~>` — string buffer right shift (Multiplicative level)

## Unicode Operator Variants

- [x] `≠` — numeric inequality (alias for `!=`)
- [x] `≤` — numeric less-than-or-equal (alias for `<=`)
- [x] `≥` — numeric greater-than-or-equal (alias for `>=`)
- [x] `⩵` — numeric equality (alias for `==`)
- [x] `⩶` — value identity (alias for `===`)
- [x] `×` — multiplication (alias for `*`)
- [x] `÷` — division (alias for `/`)
- [x] `−` — subtraction (alias for `-`)
- [x] `∅` — empty set

## Prefix Operators

- [ ] `+^` — integer bitwise negation
- [ ] `~^` — string buffer bitwise negation
- [ ] `?^` — boolean bitwise negation
- [ ] `temp` — temporize variable (restore on scope exit)
- [ ] `let` — conditional restoration (restore if block fails)

## Compound Assignment Operators

- [x] `/=` — division assignment
- [x] `%=` — modulo assignment
- [x] `**=` — exponentiation assignment
- [x] `x=` — string repetition assignment
- [x] `xx=` — list repetition assignment
- [ ] `min=` — min assignment
- [ ] `max=` — max assignment
- [ ] `.=` — mutating method call

## Grammar Constructs

- [ ] `S///` — non-destructive substitution
- [x] `tr///` / `TR///` — transliteration
- [ ] `!!!` — fatal stub operator
- [ ] `???` — admonitory stub operator
- [ ] `sink` — sink statement prefix

## Phasers

- [ ] `CHECK` — end of compile time
- [ ] `INIT` — start of runtime
- [ ] `KEEP` — block exit on success
- [ ] `UNDO` — block exit on failure
- [ ] `PRE` — precondition check
- [ ] `POST` — postcondition check
- [ ] `QUIT` — exception in supply/react
- [ ] `CLOSE` — supply close

## Meta-operators

- [ ] Generalized negation meta (`!op`) — beyond `!~~` and `!%%`
- [ ] Hyper assignment (`@a »+=» 1`)
- [ ] Triangular reduction (`[\+]`, `[\*]`, etc.)
- [ ] Sequential meta-operator (`S`)
