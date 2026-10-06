# The parse memo no longer keys scratch buffers (fixes a CI flake)

`scripts/rakuast-frontend.sh check` failed on roughly one run in fifty to a hundred on
`t/nativecall/nativecall-mvp.t`: `c_sqrt` declared `is native('m', v6)` resolved its symbol in
`libc.so.6`, as if it had been declared `native('c', v6)`. The cause was the parse memo. It keys an
entry by `(generation, ptr, len)` of the input text, which is unique only while the buffer that
text lives in is alive. `parse_sub_traits` re-parses a trait's argument list through
`format!("({paren_content})")`, a scratch `String` created and dropped inside the one parse
generation of the whole file; the next trait's scratch `String` (`('c', v6)` and `('m', v6)` have
the same length) was often handed the same address, so the memo answered it with the previous
trait's expression.

A memo key (`memo::memo_key`) is now made only for text inside the buffer the current generation
was begun for (`begin_buffer_generation(input)` in `parse_program` and `parse_program_recovering`)
or inside a permanently leaked heredoc region; any other text has no key and is parsed fresh. The
guarantee is structural, so a call site that parses a scratch buffer no longer has to remember to
open a generation of its own, and parser entry points that run outside any parse (a regex code
fragment, a role type argument) are no longer keyed by whatever generation happened to be
current. Lexical-scope generations (`begin_parse_generation`) keep their buffer.

Pinned by unit tests in `src/parser/memo.rs` that rewrite one scratch `String` in place (same
address, same length, different text) and check that neither the memo nor the expression parser
answers the second text with the first one's entry
([#12065](https://github.com/tokuhirom/mutsu/issues/12065)).
