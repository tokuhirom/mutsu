# `PrecompilationRepository.try-load` compiles a source once

`CompUnit::PrecompilationRepository::Default.try-load` (the API Pod::Load reads a file's
`$=pod` through) `EVAL`ed the whole source on every call, so loading the same file twice
redeclared the classes it declares: Pod::To::HTML's `render` of two Pod files with the same
content died `Redeclaration of symbol 'Magician'` (t/011-external, t/012-multi, t/120-templates
were short or empty). A precompilation repository compiles a unit once per source, so the
unit is now memoized by the sha1 of its text; a different source still compiles as its own
unit. Pod::To::HTML's 18 test files now give the same `ok` lines as `raku`
(three of them, 020/050/060, pass where `raku` itself fails in this environment).
