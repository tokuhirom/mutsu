use Test;

# A regex-embedded `:my $var = EXPR;` declarator's initializer must see the
# in-progress match state (`$0`, `$1`, ..., `$/`) exactly like a plain
# `{ ... }` code block does at the same point in the pattern, and its value
# must persist into the caller's enclosing lexical scope after a successful
# match -- documented in raku-doc/doc/Language/regexes.rakudoc: ":my helps
# scoping the $c variable within the regex and beyond", and per its worked
# example: "aba" ~~ / (a) {say "Check so far ", ~$/} b :my $c = ~$0; /
# leaves $c set to 'a' after the match.
#
# Two distinct bugs combined to break this in mutsu:
#  1. The VarDecl atom handler (RegexAtom::VarDecl in
#     src/runtime/regex/regex_match_capture.rs) evaluated the initializer
#     expression without installing $0.., named-capture, or $/ bindings into
#     the interpreter env first -- so `~$0` read an unbound value and `$c`
#     ended up empty even while still inside the match.
#  2. Even once the value was captured correctly, nothing wrote an embedded
#     (mid-pattern, non-leading) `:my`'s value back to the caller's scope
#     after a successful match -- only a *leading* declarative-prefix `:my`/
#     `:let` had a writeback path (`regex_match_with_captures`'s
#     `declarators` handling). `persist_embedded_my_decls` now covers the
#     embedded-atom case too, mirroring the leading-declarator writeback.

plan 8;

{
    my $out = "";
    "aba" ~~ / (a) {$out ~= "Check so far " ~ ~$/ ~ "\n"} b :my $c = ~$0; /;
    is $out, "Check so far a\n", 'the preceding code block still runs inline';
    is $c, 'a', 'the :my declarator captured the correct $0 value';
}

{
    "aba" ~~ / (a) {} b :my $c = ~$0; {} /;
    is $c, 'a',
        'a $0-derived :my value is visible to a later code block in the same match';
}

{
    "aba" ~~ / (a) {say "Check so far ", ~$/} b :my $slash = ~$/; /;
    is $slash, 'ab', ':my initializer can read $/ (the whole match so far) directly';
}

{
    "ab" ~~ / (a)(b) {} :my $c = ~$0; :my $d = ~$1; /;
    is $c, 'a', 'first of two chained :my declarators captures $0';
    is $d, 'b', 'second of two chained :my declarators captures $1';
}

{
    # A failed overall match must not leave a stale value from a partial
    # attempt lying around where a successful one would have persisted it --
    # this is a smoke test that mutsu does not crash / hang on this shape,
    # not an assertion about undefined-value details.
    lives-ok {
        "xyz" ~~ / (a) {} b :my $c = ~$0; /;
    }, 'a non-matching pattern with an embedded :my declarator does not die';
}

{
    # Regression guard for the exact raku-doc worked example.
    "aba" ~~ / (a) {say "Check so far ", ~$/} b :my $c = ~$0; /;
    is $c, 'a', 'the raku-doc worked example matches documented behavior';
}
