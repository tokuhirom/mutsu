use Test;

# A `proto`/`multi sub` family declared in a `class` or `module` body is
# lexical to that body, exactly like a plain `sub`: the package's own routines
# see it, it shadows a same-named mainline family, and the mainline one is
# untouched. mutsu got the single-`sub` baseline right and the `proto`+`multi`
# case wrong in two independent ways -- the class-body family was unreachable
# from the class's own methods, and the module-body one had its candidates
# *merged* with the mainline family's, answering "Ambiguous call".
#
# Every expectation below was measured against Rakudo v2026.06 (2026-09-04)
# first; raku is the oracle and this file passes verbatim under both.

plan 13;

# --- 1. the single-sub baseline (was already correct; guard it) -------------
{
    sub p1() { "mainline" }
    class C1 {
        sub p1() { "in-class" }
        method m() { p1() }
    }
    is C1.m(), "in-class", "a class-body single sub is visible to its methods";
    is p1(), "mainline", "the mainline single sub is untouched";
}

# --- 2. a class-body proto+multi family is visible to the class's methods ---
{
    class C2 {
        proto p2($) {*}
        multi p2(Int $x) { "in-class-Int" }
        multi p2(Str $x) { "in-class-Str" }
        method m() { (p2(5), p2("a")).join("|") }
    }
    is C2.m(), "in-class-Int|in-class-Str",
        "a class-body proto+multi family is reachable from a method";
}

# --- 3. ... and it shadows a same-named mainline family --------------------
{
    sub p3($x) { "mainline" }
    class C3 {
        proto p3($) {*}
        multi p3(Int $x) { "in-class" }
        method m() { p3(5) }
    }
    is C3.m(), "in-class", "the class-body family wins over the mainline sub";
    is p3(5), "mainline", "the mainline sub is untouched";
}

# --- 4. the module-body twin ------------------------------------------------
{
    module M4 {
        proto p4($) {*}
        multi p4(Int $x) { "in-module" }
        our sub go() { p4(5) }
    }
    proto p4($) {*}
    multi p4(Int $x) { "mainline" }
    is M4::go(), "in-module", "a module-body family is not merged with the mainline one";
    is p4(5), "mainline", "the mainline family still answers outside the module";
}

# --- 5. nested packages each keep their own family --------------------------
{
    module M5 {
        proto p5($) {*}
        multi p5(Int $x) { "M5" }
        module N {
            proto p5($) {*}
            multi p5(Int $x) { "M5::N" }
            our sub go() { p5(5) }
        }
        our sub go() { p5(5) }
    }
    is M5::N::go(), "M5::N", "the innermost package's own family wins";
    is M5::go(), "M5", "the enclosing package keeps its own family";
}

# --- 6. a package body with NO proto of its own EXTENDS the outer family ----
# This is the case that must keep merging: raku gives a `multi` declared with
# no proto in scope the innermost visible proto's candidate list.
{
    proto p6($) {*}
    multi p6(Int $x) { "mainline-Int" }
    module M6 {
        multi p6(Str $x) { "in-module-Str" }
        our sub go() { (p6(5), p6("a")).join("|") }
    }
    is M6::go(), "mainline-Int|in-module-Str",
        "a module-body multi with no proto of its own extends the outer family";
    is p6(5), "mainline-Int", "the outer family is unchanged";
}
{
    proto p7($) {*}
    multi p7(Int $x) { "mainline-Int" }
    class C7 {
        multi p7(Str $x) { "in-class-Str" }
        method m() { (p7(5), p7("a")).join("|") }
    }
    is C7.m(), "mainline-Int|in-class-Str",
        "a class-body multi with no proto of its own extends the outer family";
}

# --- 7. a mainline family stays reachable from a class with no family ------
{
    proto p8($) {*}
    multi p8(Int $x) { "mainline" }
    class C8 { method m() { p8(5) } }
    is C8.m(), "mainline", "a class with no family of its own sees the mainline one";
}
