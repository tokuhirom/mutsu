use lib $*PROGRAM.parent.child("lib").Str;
use Test;
use InterpConstructOtf;

# Pins that module subs containing formerly interpreter-gated constructs
# (nested routine decls, CATCH/CONTROL, phasers, subtest, start, once,
# nested class/role/grammar decls, and EVAL/EVALFILE) behave raku-identically
# when OTF-compiled (def_is_otf_compilable_module_single).

plan 40;

is oc-nested(42), "int:42|str:s", "nested sub decl with when+return";
is oc-nested-when(7), "Iafter;", "nested sub when does not escape the nested routine";
is oc-proto(5), "int:5", "nested proto+multi dispatch (Int)";
is oc-proto("x"), "str:x", "nested proto+multi dispatch (Str)";
is oc-token("hello world"), "hello", "nested token decl matches";

is oc-catch(1), "caught: boom 1", "block-level CATCH catches die";
is oc-catch-top(1), "caught: boom 1", "body-level CATCH return from handler";
is oc-catch-top(0), "no-die", "body-level CATCH keeps normal-path return value";
is oc-control(), "w1,resumed", "CONTROL handler resumes a warn";

is oc-phaser(3), "ret", "phaser sub returns body value";
is oc-phaser-read(), "enter;body:3;leave;", "ENTER/LEAVE order observed via outer lexical";

is oc-once(), 1, "once fires on first call";
is oc-once(), 1, "once does not fire on second call";

is oc-start(5), 210, "start captures param, local and module lexical";
my @r = await (^4).map: { start oc-catch-in-thread($_) };
is @r.sort.join("|"), "caught:t0|caught:t2|ok:1|ok:3", "CATCH inside start-threaded calls";
await (^8).map: { start oc-leave-count() };
is oc-leave-read(), 8, "LEAVE fires once per call across threads";

is oc-class(3, 4), 7, "nested my class with attrs and method";
is oc-class-shape-a(), "a-shape", "same-named nested class (a)";
is oc-class-shape-b(), "b-shape", "same-named nested class (b)";
is oc-class-shape-a(), "a-shape", "no cross-sub class pollution after b";
is oc-class-capture(), 3, "nested class method mutates captured lexical";
is oc-class-capture(), 3, "captured lexical is fresh per call";
is oc-class-inherit(), "derived+base", "nested class inheritance with callsame";
is oc-class-recur(4), 10, "recursive sub with nested class decl";
is oc-role(), 10, "parameterized nested role mixed into nested class";
is oc-grammar("123"), "match:123", "nested grammar parses";
is oc-grammar("ab"), "nomatch", "nested grammar rejects";

is oc-eval-param(41), 42, "EVAL reads the sub's param";
is oc-eval-lexical(), 20, "EVAL reads the sub's lexical";
is oc-eval-write(), 5, "EVAL writes the sub's lexical";
is oc-eval-decl-sub(), 107, "EVAL declares and calls a sub";
is oc-eval-caller-d3(), 17, "EVAL CALLER:: depth 3 reaches the invoking sub";
is oc-eval-caller-d1(), "Nil", "EVAL CALLER:: depth 1 is the EVAL frame (Nil)";
is oc-eval-topic(), "10,20,30", "EVAL sees the loop topic";
is oc-eval-sibling(), "sekrit!", "EVAL calls a module-private sibling sub";
is oc-eval-nested("abc"), "abc-wrapped", "EVAL inside a nested my sub sees captures";
is oc-eval-catch(), "caught:boom", "CATCH catches a die from EVAL'd code";
is oc-evalfile($*PROGRAM.parent.child("lib/evalfile-fixture.raku").Str), 42,
    "EVALFILE runs a file and returns its last value";
# second call: the OTF-compiled body is cached; EVAL re-evaluates per call
is oc-eval-write(), 5, "EVAL lexical write is fresh on a second call";

oc-subtest("subtest inside a module sub", 3, 3);
