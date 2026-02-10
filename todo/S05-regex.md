# S05 - Regexes and Rules

Reference: `old-design-docs/S05-regex.pod`

Covers regex syntax, grammars, longest-token matching, and match objects.

---

## Core Regex Features

### Match Variables
- [x] `$/` match object (lexically scoped)
- [x] Captures start at `$0` (not `$1`)
- [ ] `$/` binding via `:=`

### Unchanged Features
- [x] Capturing groups `(...)`
- [x] Quantifiers `*`, `+`, `?`
- [x] Alternatives `|`
- [x] Backslash escape `\`
- [ ] Minimal matching `??`, `*?`, `+?`

### Simplified Lexical Parsing
- [x] Identifiers literal by default
- [x] Non-identifiers metasyntactic by default
- [ ] Single quotes for literal matching inside regex
- [ ] Double quotes for interpolated matching

---

## Modifiers

- [x] `:i` / `:ignorecase`
- [ ] `:ii` / `:samecase` (substitution)
- [ ] `:m` / `:ignoremark` (ignore diacriticals)
- [ ] `:mm` / `:samemark` (substitution)
- [x] `:g` / `:global`
- [x] `:r` / `:ratchet` (no backtracking, used in token)
- [ ] `:c` / `:continue` (continue from position)
- [ ] `:p` / `:pos` (match only at position)
- [x] `:s` / `:sigspace` (significant whitespace, used in rule)
- [ ] `:ss` / `:samespace` (substitution)
- [ ] `:bytes`, `:codes`, `:graphs`, `:chars` (Unicode level)
- [ ] `:Perl5` / `:P5` compatibility mode
- [ ] `:x(N)` repetition count
- [ ] `:nth(N)` find Nth occurrence
- [ ] `:ov` / `:overlap`
- [ ] `:ex` / `:exhaustive`
- [ ] `:rw` read-write mode
- [ ] `:dba` error message naming
- [ ] Variable declarations `:my`, `:our`, `:state`

---

## Metacharacters

### Changed
- [x] `.` matches any character including newline
- [x] `^` and `$` match string start/end
- [x] `\n` matches logical newline

### New
- [ ] `#` as comment in regex (default `/x`)
- [ ] Whitespace is metasyntactic (not literal)
- [ ] `^^` and `$$` line boundaries (zero-width)
- [ ] `\N` anything except newline
- [ ] `&` and `&&` conjunctive terms
- [ ] `~~` and `!~~` submatch operators
- [ ] `~` nested subrule terminator helper
- [x] `<...>` extensible metasyntax

---

## Bracket Rationalization

- [x] `(...)` capturing group
- [x] `[...]` non-capturing group
- [x] `<[...]>` character class
- [x] `{...}` embedded closure
- [x] `make` function (generate AST)
- [x] `made` method (access AST from Match)
- [ ] `**` general repetition (`** 2..5`)
- [ ] `%` separator modifier on quantifiers
- [ ] `%%` trailing separator modifier

---

## Variable Interpolation in Regex

- [ ] `$var` matches literally (like `\Q...\E`)
- [ ] `<$var>` for Regex object interpolation
- [ ] `@array` as alternation
- [ ] Proto regex with multi dispatch

---

## Extensible Metasyntax (`<...>`)

### Subrule Calls
- [x] `<ident>` capturing subrule call
- [x] `<.ws>` non-capturing method call (dot prefix)
- [ ] `<&func>` lexical routine call
- [ ] `<$var>` indirect subrule (variable contains Regex)
- [ ] `<::($name)>` symbolic indirect subrule

### Aliasing
- [x] `<name=rule>` aliased subrule call
- [ ] `$<key>=(...)` scalar aliasing
- [ ] `@<key>=(...)` array aliasing
- [ ] `%<key>=(...)` hash aliasing

### Assertions
- [ ] `<?{code}>` code assertion (positive)
- [ ] `<!{code}>` code assertion (negative)
- [ ] `<?before pattern>` lookahead
- [ ] `<?after pattern>` lookbehind
- [ ] `<!before pattern>` negative lookahead
- [ ] `<!after pattern>` negative lookbehind
- [ ] `<?>` always true
- [ ] `<!>` always false
- [ ] `<?at($pos)>` position assertion

### Character Classes
- [x] `<[a..z]>` enumerated character class with ranges
- [x] `<-[abc]>` complemented character class
- [x] `<+[abc]>` positive character class
- [ ] Character class combinations (additive/subtractive)
- [ ] Unicode properties (`:Letter`, `:Digit`, etc.)
- [ ] Set operators in classes (`&`, `|`, `^`)

### Match Span Markers
- [ ] `<(` and `)>` to set match `.from` and `.to`
- [ ] `<<` and `>>` / `<<` and `>>` word boundaries

### Predefined Subrules
- [x] `<alpha>`, `<digit>`, `<alnum>`
- [x] `<upper>`, `<lower>`
- [ ] `<ident>` (full identifier matching)
- [ ] `<xdigit>`, `<print>`, `<graph>`, `<cntrl>`, `<punct>`
- [x] `<ws>` whitespace
- [ ] `<space>`, `<blank>`
- [ ] `<wb>` word boundary
- [ ] `<ww>` between word chars

---

## Backslash Sequences

- [x] `\d` digit, `\D` non-digit
- [x] `\w` word char, `\W` non-word
- [x] `\s` whitespace, `\S` non-whitespace
- [ ] `\h` horizontal whitespace, `\H` negation
- [ ] `\v` vertical whitespace, `\V` negation
- [x] `\n` newline, `\N` non-newline
- [ ] `\t` tab, `\T` non-tab
- [ ] `\r` return, `\R` non-return

---

## Backtracking Control

- [ ] `:?` or `?` frugal matching
- [ ] `:!` greedy in frugal context
- [ ] `:` no backtracking (possessive)
- [ ] `::` cut in LTM alternation
- [ ] `:::` cut for entire regex
- [ ] `<commit>` commit entire match
- [ ] `<cut>` logically delete matched prefix

---

## Regex Declarators

- [x] `regex { pattern }` (backtracking)
- [x] `rx / pattern /` (slash-delimited)
- [x] `token { pattern }` (ratchet, no backtracking)
- [x] `rule { pattern }` (ratchet + sigspace)
- [x] `m//` match operator
- [x] `s///` substitution operator

---

## Longest-Token Matching (LTM)

- [x] `|` logical alternation with LTM
- [ ] `||` temporal alternation (sequential, left-to-right)
- [x] LTM tiebreaker: longest match, then longest literal prefix
- [ ] Sequence points (terminate token pattern)
- [ ] Lookahead/lookbehind effects on LTM

---

## Match Objects

- [x] Boolean context (success/failure)
- [x] String context (matched substring)
- [x] Array context (positional captures)
- [ ] Hash context (named captures as hash)
- [x] `.from` initial position
- [x] `.to` final position
- [ ] `.chars` length
- [ ] `.orig` original string
- [x] `.Str` matched substring
- [x] `.made` / `.ast` abstract syntax tree
- [ ] `.caps` sequential captures as pairs
- [ ] `.chunks` captures with interleaved text
- [ ] `.prematch`, `.postmatch`

### Subpattern Captures
- [x] Hierarchical capture (`$0`, `$1`, ...)
- [ ] Quantified captures return array of Matches
- [ ] Item quantifier `?` returns single or Nil

### Subrule Captures
- [x] Named captures (`$<name>`)
- [ ] Multiple same-name subrule creates array
- [ ] `@<name>` forces array for single match

---

## Grammars

- [x] `grammar Name { ... }` declaration
- [ ] `is ParentGrammar` inheritance
- [x] Token/rule declarations inside grammar
- [ ] `.parse($string)` on grammar
- [ ] `:actions($object)` for action methods
- [ ] `:rule` to specify entry point
- [ ] `.subparse(:pos)` for partial parsing
- [x] `proto token` with LTM
- [x] `make` / `made` for AST construction

### Action Classes
- [ ] Methods correspond to rule names
- [ ] Receive Match object parameter
- [ ] Called when rule matches successfully

---

## Substitution and Transliteration

### Substitution
- [x] `s/pattern/replacement/`
- [ ] `.subst(/pattern/, replacement)` method form
- [ ] `:g`, `:i` and other adverbs as named arguments
- [ ] Closure replacement: `s/pattern/{computed}/`

### Transliteration
- [ ] `tr///` operator
- [ ] `.trans()` method form
- [ ] `:samecase` / `:samemark` variants
- [ ] Regex form for pattern-based translation
