# Fixture for t/routines/closure/enum-name-not-leaked-to-foreign-code.t.
# Mirrors PDF::Grammar: a grammar whose body declares an enum with the member
# name `array`, plus a module-level routine that calls back into user code.
grammar EnumLeakGrammar {
    enum AST-Types is export(:AST-Types) <array body>;
    rule TOP { <array> }
    rule array { 'x' }
}

sub call-back(&code) is export { code() }
