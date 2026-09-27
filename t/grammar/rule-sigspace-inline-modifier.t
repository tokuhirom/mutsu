use Test;

plan 9;

# An inline `:!s` / `:!sigspace` inside a `rule` turns sigspace off for the
# rest of the enclosing group, so the whitespace after the adverb and between
# the group's atoms is layout, not an implicit `<.ws>` (ANTLR4::Grammar's
# `rule grammarType { ( :!sigspace 'lexer' | 'parser' )? 'grammar' }`).
grammar Kind {
    rule TOP { ( :!sigspace 'lexer' | 'parser' )? 'grammar' }
}
is ~Kind.parse('lexer grammar')[0], 'lexer', ':!sigspace keeps trailing whitespace out of the capture';
is ~Kind.parse('parser grammar')[0], 'parser', 'the adverb covers every alternative of its group';

grammar Adjacent {
    rule TOP { [ :!s 'a' 'b' ] 'c' }
}
nok Adjacent.parse('a b c'), 'whitespace between atoms is not significant after :!s';
ok Adjacent.parse('ab c'), 'sigspace resumes after the group closes';

grammar Scoped {
    rule TOP { ( :!s 'x' ) 'y' 'z' }
}
ok Scoped.parse('x y z'), 'the adverb does not leak past its group';
is ~Scoped.parse('x y z')[0], 'x', 'capture of the :!s group is exactly its text';

grammar Reenabled {
    rule TOP { [ :!s 'a' [ :s 'b' 'c' ] 'd' ] }
}
# The inner group's whitespace (including before its `]`) is significant; the
# outer `:!s` still governs the `'a'` .. `[` gap.
ok Reenabled.parse('ab c d'), 'an inner :s turns sigspace back on for its own group';
nok Reenabled.parse('a b c d'), 'the outer :!s still applies outside the inner group';

# A `'#'` literal in a rule is a quoted atom, not the start of a comment.
grammar Label {
    rule TOP { <e> [ '#' <label=ID> ]? ';' }
    token e { \w+ }
    token ID { \w+ }
}
is ~Label.parse('a # L ;')<label>, 'L', "a quoted '#' is matched, not treated as a comment";
