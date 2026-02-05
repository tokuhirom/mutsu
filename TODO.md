# TODO - S02-bits.pod Features

Based on `/home/tokuhirom/work/old-design-docs/S02-bits.pod`

## Lexical Conventions

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

## Built-In Data Types

### Numeric Types
- [x] Int
- [x] FatRat
- [ ] Rat (rational numbers)
- [ ] Num (floating point)
- [ ] Complex

### Other Types
- [x] Str
- [x] Bool
- [x] Array
- [x] Range (`..`, `..^`)
- [ ] Hash
- [ ] Set, Bag, Mix
- [ ] Nil (partial)

## Literals

### Numeric Literals
- [x] Integer literals
- [ ] Underscores in numbers (`1_000_000`)
- [ ] Radix markers (`0x`, `0o`, `0b`)
- [ ] General radices (`:16<DEAD_BEEF>`)
- [ ] Exponentials (`1e10`)
- [ ] Rational literals (`1/3`)
- [ ] Complex literals (`1+2i`)

### String Literals
- [x] Double-quoted strings with interpolation
- [x] Single-quoted strings
- [ ] Q forms (`Q`, `q`, `qq`)
- [ ] Heredocs (`q:to/END/`)
- [ ] Angle quotes (`<word list>`)

## Variables and Names

### Sigils
- [x] `$` (scalar)
- [x] `@` (array)
- [ ] `%` (hash)
- [ ] `&` (code)

### Twigils
- [x] `*` (dynamic/global: `$*PID`, `$*USER`, etc.)
- [ ] `?` (compile-time: `$?FILE`, `$?LINE`)
- [ ] `!` (attribute)
- [ ] `.` (public attribute)
- [ ] `^` (placeholder/positional)
- [ ] `:` (placeholder/named)

## Operators (see S03)

- [x] Basic arithmetic (`+`, `-`, `*`, `/`, `%`)
- [x] Comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`)
- [x] String comparison (`eq`, `ne`, `lt`, `gt`, `le`, `ge`)
- [x] Logical (`&&`, `||`, `!`, `and`, `or`, `not`)
- [x] String concatenation (`~`)
- [x] Range (`..`, `..^`)
- [x] Smartmatch (`~~`)
- [ ] Ternary (`?? !!`)
- [ ] Assignment variants (`+=`, `-=`, etc.)
- [ ] Repetition (`x`, `xx`)

## Control Flow

- [x] `if`/`elsif`/`else`
- [x] `unless`
- [x] `while`/`until`
- [x] `for`
- [x] `loop`
- [ ] `given`/`when`
- [ ] `repeat while`/`repeat until`

## Subroutines and Methods

- [x] `sub` declarations
- [x] Basic parameter handling
- [ ] Named parameters
- [ ] Default values
- [ ] Type constraints
- [ ] `method` declarations
- [ ] Multi dispatch

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

## Priority

High priority items for roast test compatibility:
1. Hash support (`%`)
2. Ternary operator (`?? !!`)
3. `given`/`when`
4. `throws-like`, `lives-ok`, `dies-ok`
5. EVAL
