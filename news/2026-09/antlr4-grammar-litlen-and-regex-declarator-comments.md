# ANTLR4::Grammar: litlen from the NFA, and `#|{ }` comments in regexes

ANTLR4::Grammar 0.6.3's two remaining red test files both came down to Rakudo
behaviour mutsu had not copied.

**The longest-literal tie-break (`litlen`) is now measured by the LTM NFA.**
When two `|` branches have equally long declarative prefixes, Rakudo ranks them
by the longest literal each one matched. NQP decides which literals count while
it builds each rule's NFA: a literal counts only in the leading literal run of
*its own* rule body. A subrule call keeps its callee's counting literals
wherever it sits, even inside a quantified group. A literal written after the
call does not count. mutsu measured this with a separate walk that stopped at
the first quantifier, and ADR-0022 listed that as an accepted divergence.
ANTLR4::Grammar's `LEXER_CHAR_SET_RANGE`, `[<ELEM_NO_HYPHEN> '-']? <ELEM>`, is
exactly that shape. Its `\uXXXX` escapes end both prefixes at the same
`** {4}` fate, and the range branch should then win by declaration order. mutsu
gave the range branch no litlen, so `[\u000a-\u000c]` came out as three
elements. The NFA builder now marks the literals NQP would mark, and the run
records the furthest one crossed, as MoarVM's `longlit` does. The separate walk
is gone.

**Bracketed comments may span lines in a regex.** A regex's whitespace is the
main language's, so the declarator blocks `#|{ ... }` and `#={ ... }` are
comments there too, nested brackets included. mutsu's regex scanners only knew
``#`{ }``, so they read `#|{` as a line comment and took the comment's closing
`}` as the end of the token. The generated grammars for RFilter.g4, SQLite.g4
and Python3.g4 carry their source actions in such comments. The scanners now
call the main language's comment skipper.

ANTLR4::Grammar: `t/10-basic-grammar.t` and `t/03-corpus-compile.t` pass, so all
13 baseline files are at parity.
