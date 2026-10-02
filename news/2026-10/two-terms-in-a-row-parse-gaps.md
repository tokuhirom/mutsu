# Nineteen statement shapes that stopped parsing halfway now parse whole

The ecosystem ledger's largest parse-failure cluster (#7988) had become
`Confused. Two terms in a row`. That is the check that refuses to split a
statement silently when the parser stopped short of its end. Behind the one
message were nineteen distinct constructs, each found in a real
distribution, that rakudo parses and mutsu did not:

- `(0 xx 2) ++ @a` and `1 -- 2`: a spaced doubled operator is the infix
  followed by a prefix (Math::GameTheory).
- `for @css -> $css, { ... }`: a trailing comma ends a pointy parameter list
  (CSS::Properties).
- `with 1, 2, 3 { ... }` / `if 1, 0 { ... }`: a statement condition is a
  full expression, comma included (P5reverse).
- `my enum <lx ly ux uy>`: an anonymous `my` enum (PDF::Content, LibXML).
- `method!name { ... }` with no space (Manifest::StopWar).
- `retry { ... }` and `try retry { ... }` calling a sub declared later in the
  file (Pakku, Pod::To::HTML).
- `$obj.attr mod= 7`: a word operator's `op=` on an rw accessor
  (Timezones::ZoneInfo).
- `my T $x .= new(...).later: ...`: a postfix chain after `.=` applies to the
  new value and is sunk (CSS::Stylesheet, Spreadsheet::Libxlsxio).
- `Str = Str`: a type-only parameter with a default (SQL::Abstract).
- `call with $x` / `call without $x` as statement modifiers after a bare
  call (Compress::LZString).
- `last $res`: the routine form of `last` (MCP).
- `class :: does R[...] { }.new`: a class expression composing a
  parameterized role (DB::Xoos).
- `has $.p handles @names` (Config::BINDish).
- `my %h is env(:sep<:>, :kvsep<=>)`: a variable trait's argument is a whole
  list (Trait::Env).
- `nok($x) xx 2`: a parenthesized test call is an expression operand
  (Terminal::LineEditor).
- `orwith EXPR -> $s is copy { ... } elsif ...` (REPL).
- `<[ \x[0000] .. \x[10FFFF] ] - [ " \\ ]>`: a bracketed escape inside a
  character class no longer ends the bracket group early (Grammar::Modelica).
- `my T $.attr .= new(...)` at class level (RPi::Device::ST7036).
- `role Exception is ::Exception` (SQL::Abstract).

`t/lang/parsing/two-terms-statement-shapes-split.t` pins each one against
rakudo's answer.
