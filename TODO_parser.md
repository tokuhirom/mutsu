# TODO: Parser — Missing Operators & Grammar

## Infix Operators

- [x] `xx` — list repetition (Replication level)
- [x] `o` / `∘` — function composition (Concatenation level)
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
- [x] `=~=` / `≅` — approximately equal (Chaining level)
- [x] `=:=` — container identity (Chaining level)
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

- [x] `+^` — integer bitwise negation
- [ ] `~^` — string buffer bitwise negation
- [x] `?^` — boolean bitwise negation
- [ ] `temp` — temporize variable (restore on scope exit)
- [ ] `let` — conditional restoration (restore if block fails)

## Compound Assignment Operators

- [x] `/=` — division assignment
- [x] `%=` — modulo assignment
- [x] `**=` — exponentiation assignment
- [x] `x=` — string repetition assignment
- [x] `xx=` — list repetition assignment
- [x] `min=` — min assignment
- [x] `max=` — max assignment
- [x] `.=` — mutating method call

## Grammar Constructs

- [x] `S///` — non-destructive substitution
- [x] `tr///` / `TR///` — transliteration
- [x] `!!!` — fatal stub operator
- [x] `???` — admonitory stub operator
- [x] `sink` — sink statement prefix

## Phasers

- [x] `CHECK` — end of compile time
- [x] `INIT` — start of runtime
- [x] `KEEP` — block exit on success
- [x] `UNDO` — block exit on failure
- [x] `PRE` — precondition check (parsed, no-op)
- [x] `POST` — postcondition check (parsed, no-op)
- [x] `QUIT` — exception in supply/react (parsed, no-op)
- [x] `CLOSE` — supply close (parsed, no-op)

## Meta-operators

- [ ] Generalized negation meta (`!op`) — beyond `!~~` and `!%%`
- [ ] Hyper assignment (`@a »+=» 1`)
- [ ] Triangular reduction (`[\+]`, `[\*]`, etc.)
- [ ] Sequential meta-operator (`S`)
