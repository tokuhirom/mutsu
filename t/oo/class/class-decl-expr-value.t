use v6;
use Test;
use lib 't/lib';
use EvalContext;

# A `class Name { ... }` used as an EXPRESSION must evaluate to the type
# object the declaration just created -- never to a bareword lookup of
# `Name`. A bareword lookup can resolve to a completely unrelated,
# same-named class from a different scope (see
# news/2026-08/class-decl-expr-is-not-a-name-lookup.md).

plan 5;

# 1. Mainline, no conflicting same-named class: baseline sanity.
subtest 'mainline, no conflict' => {
    plan 1;
    my $t = (class MainlineFoo { has $.x = 42 });
    is $t.new.x, 42, 'class-decl-as-expr yields a usable type object';
}

# 2. Inside a routine, no conflicting same-named class.
subtest 'inside a routine, no conflict' => {
    plan 1;
    sub make-it() {
        return (class RoutineFoo { has $.y = 7 });
    }
    is make-it().new.y, 7, 'class-decl-as-expr inside a sub body works';
}

# 3. A same-named class already exists in an ENCLOSING scope (a `module`),
# and the expression-position declaration runs inside that module: current
# package prefixing means the two are genuinely different registry entries
# even though they share a bare name -- the expression must yield the NEW
# one, not the pre-existing outer one.
class SameNameOuter { }
subtest 'same-named class in enclosing scope, nested module' => {
    plan 2;
    module SameNameHolder {
        my $inner = (class SameNameOuter { has $.w = 99 });
        is $inner.new.w, 99,
            'nested-module class-decl-as-expr yields the NEW class';
    }
    isa-ok SameNameOuter.new, SameNameOuter,
        'the outer same-named class is untouched';
}

# 4. Inside EVAL'd code that runs in a DIFFERENT compilation unit (a `sub`
# defined in another module -- t/lib/EvalContext.rakumod), while a
# same-named class already exists at the caller's mainline scope. This is
# the exact shape roast/S12-class/attributes.t's "HOW on attributes lives,
# custom class" subtest hits via Test.rakumod's `eval-lives-ok`.
class SameNameEvalOuter { }
subtest 'same-named class, EVAL in a different compilation unit' => {
    plan 2;
    my $got = run-plain(
        '(class SameNameEvalOuter { has $.z = 123 }).new.z'
    );
    is $got, 123,
        'EVAL-in-another-unit class-decl-as-expr yields the NEW class';
    isa-ok SameNameEvalOuter.new, SameNameEvalOuter,
        'the outer same-named class is still untouched';
}

# 5. An UNNAMED (anonymous) class-decl expression still works -- the fix
# must not regress the existing `(class { ... })` / `class :: { ... }` path.
subtest 'anonymous class-decl expression' => {
    plan 1;
    my $t = (class { has $.v = 5 });
    is $t.new.v, 5, 'anonymous class-decl-as-expr still works';
}
