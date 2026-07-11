unit module InterpConstructOtf;

# Constructs formerly excluded from module-sub OTF compilation
# (def_is_otf_compilable_module_single). Each sub exercises one construct;
# t/module-sub-otf-interpreter-constructs.t pins raku-identical behavior.

# nested sub decl + when non-local control flow (is-deeply-junction pattern)
our sub oc-nested($x) is export {
    sub guts($v) {
        given $v {
            when Int { return "int:$v" }
            when Str { return "str:$v" }
        }
        return "other";
    }
    guts($x) ~ "|" ~ guts("s");
}

# nested when without return: control must not escape the nested routine
our sub oc-nested-when($x) is export {
    my $r = "";
    sub inner($v) {
        given $v {
            when Int { $r ~= "I"; }
            when Str { $r ~= "S"; }
        }
        $r ~= "after;";
    }
    inner($x);
    $r;
}

# nested proto + multi decls
our sub oc-proto($x) is export {
    proto sub inner($) {*}
    multi sub inner(Int $v) { "int:$v" }
    multi sub inner(Str $v) { "str:$v" }
    inner($x);
}

# nested token decl used via regex match
our sub oc-token($s) is export {
    my token word { \w+ }
    ($s ~~ /<word>/) ?? ~$/<word> !! "no-match";
}

# block-level CATCH handler
our sub oc-catch($x) is export {
    my $r = "start";
    {
        die "boom $x";
        CATCH { default { $r = "caught: " ~ .message; } }
    }
    $r;
}

# body-level CATCH with return from the handler; the non-dying path must
# still return the body's last value (tree-walk used to lose it)
our sub oc-catch-top($x) is export {
    CATCH { default { return "caught: " ~ .message } }
    die "boom $x" if $x;
    "no-die";
}

# CONTROL handler (warn + resume)
our sub oc-control() is export {
    my @msgs;
    {
        warn "w1";
        @msgs.push("resumed");
        CONTROL { when CX::Warn { @msgs.push(.message); .resume } }
    }
    @msgs.join(",");
}

# ENTER/LEAVE phasers observed via an outer lexical
my $phaser-log = "";
our sub oc-phaser($x) is export {
    ENTER { $phaser-log ~= "enter;" }
    LEAVE { $phaser-log ~= "leave;" }
    $phaser-log ~= "body:$x;";
    "ret";
}
our sub oc-phaser-read() is export { $phaser-log }

# once block: fires exactly once across calls (within a thread)
my $once-count = 0;
our sub oc-once() is export {
    once { $once-count++ }
    $once-count;
}

# start block capturing a param, a local, and a module lexical. NOTE: a body
# with `start` stays on the interpreter fallback (recursive start captures
# break under OTF — t/start-block-return-value.t test 3); this pins the
# behavior either way.
my $mod-lex = 100;
our sub oc-start($x) is export {
    my $local = $x + $mod-lex;
    my $p = start { $local * 2 };
    await $p;
}

# CATCH inside a sub called from start-threads
our sub oc-catch-in-thread($x) is export {
    CATCH { default { return "caught:" ~ .message } }
    die "t$x" if $x %% 2;
    "ok:$x";
}

# LEAVE phaser accumulation across threads
my $leave-lock = Lock.new;
my $leave-count = 0;
our sub oc-leave-count() is export {
    LEAVE { $leave-lock.protect: { $leave-count++ } }
    1;
}
our sub oc-leave-read() is export { $leave-count }

# nested `my class` with attributes, a method, and per-call construction
our sub oc-class($x, $y) is export {
    my class Point {
        has $.x;
        has $.y;
        method sum() { $!x + $!y }
    }
    Point.new(:$x, :$y).sum;
}

# two subs declaring the SAME nested class name — must not pollute each other
our sub oc-class-shape-a() is export {
    my class Shape { method kind() { "a-shape" } }
    Shape.new.kind;
}
our sub oc-class-shape-b() is export {
    my class Shape { method kind() { "b-shape" } }
    Shape.new.kind;
}

# nested class method mutating a captured lexical (fresh per call)
our sub oc-class-capture() is export {
    my $count = 0;
    my class Bumper { method bump() { $count++ } }
    my $b = Bumper.new;
    $b.bump; $b.bump; $b.bump;
    $count;
}

# inheritance between two nested classes + callsame
our sub oc-class-inherit() is export {
    my class Base { method greet() { "base" } }
    my class Derived is Base { method greet() { "derived+" ~ callsame } }
    Derived.new.greet;
}

# recursive sub declaring a class
our sub oc-class-recur($n) is export {
    my class Node { has $.v }
    return Node.new(v => 0).v if $n <= 0;
    oc-class-recur($n - 1) + Node.new(v => $n).v;
}

# parameterized role mixed into a nested class
our sub oc-role() is export {
    my role Sized[$max] { method max() { $max } }
    my class Box does Sized[10] { }
    Box.new.max;
}

# grammar declared inside a sub
our sub oc-grammar($s) is export {
    my grammar NumG {
        token TOP { \d+ }
    }
    NumG.parse($s) ?? "match:$s" !! "nomatch";
}

# subtest inside a module sub
use Test;
our sub oc-subtest($desc, $a, $b) is export {
    subtest $desc => {
        plan 2;
        ok $a == $b, "values equal";
        is $a, $b, "is equal";
    }
}
