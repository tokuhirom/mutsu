# S15 - Unicode

Reference: `old-design-docs/S15-unicode.pod`

Covers string normalization, Unicode properties, and character operations.

---

## String Base Units

- [x] Grapheme-based string operations (`.chars`)
- [ ] Codepoint-based operations (`.codes`)
- [ ] Code unit-based operations
- [ ] Byte-based operations

---

## Normalization Forms

- [ ] NFG (Perl 6 default - Normal Form Grapheme)
- [ ] NFC (Normal Form Composed)
- [ ] NFD (Normal Form Decomposed)
- [ ] NFKC (Compatibility Composed)
- [ ] NFKD (Compatibility Decomposed)
- [ ] NFC/NFD/NFKC/NFKD type objects

---

## Str Type

- [x] NFG form strings (conceptually)
- [ ] `.codes` codepoint count
- [ ] `.encode(encoding)` to Buf
- [ ] Immutable string semantics

---

## Uni Type

- [ ] Mixed normalization form strings
- [ ] Codepoint-level operations

---

## Unicode Information

### Property Lookup
- [ ] `uniprop(codepoint, property)` function
- [ ] `unibool(codepoint, property)` boolean property
- [ ] `unimatch(codepoint, category)` category check

### Character Operations
- [ ] `ord($str)` / `.ord` method
- [ ] `chr($codepoint)` / `.chr` method
- [ ] `ords($str)` / `.ords` method
- [ ] `chrs(@codepoints)` / `.chrs` method

### Character Names
- [ ] `uniname($codepoint)` Unicode character name
- [ ] `uninames($str)` names for all characters

### Numeric Values
- [ ] `unival($codepoint)` Unicode numeric value
- [ ] `univals($str)` values for all characters

---

## Buf Type

- [ ] Buffer type for raw bytes
- [ ] `.decode(encoding)` to Str
- [ ] `buf8`, `buf16`, `buf32`, `buf64` native types
- [ ] Encoding/decoding support (UTF-8, UTF-16, Latin-1, ASCII)

---

## Regex Unicode Support

- [ ] Default grapheme-level matching
- [ ] Normalization adverbs in regex
- [ ] Grapheme explosion syntax (`<C/pattern/>`, `<D/pattern/>`)

---

## Quoting Constructs

- [ ] `:nfd`, `:nfc`, `:nfkd`, `:nfkc` adverbs on quotes

---

## Unicode Literals

- [x] Unicode identifiers (partial)
- [ ] Unicode numeric literals
- [ ] Full Unicode operator support

---

## Pragmas

- [ ] `use encoding` for default encoding
- [ ] `use unicode :v(version)` for Unicode version
