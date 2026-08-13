use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 2;

# Regression: ADR-0019 Phase E box E11. `value_raku_repr`, the diagnostic
# formatter behind `is-deeply`/`is-eqv`'s "expected:"/"got:" lines, used to
# call `native_method_0arg` directly -- which never recognizes a
# user-defined `.raku` override on an `Instance`, so the diagnostic silently
# fell back to a generic stringification instead of the user's own `.raku`.
# Verified against real raku 2026-08-14: it prints the user-defined `.raku`.

is_run
    'use Test; class Foo { has $.x; method raku { "MyFoo(" ~ $.x ~ ")" } };'
    ~ 'is-deeply Foo.new(x=>1), Foo.new(x=>2);',
    {
        :out(/'expected: MyFoo(2)' .+ 'got: MyFoo(1)'/),
        :1status,
    },
    'is-deeply diagnostic honors a user-defined .raku override';

is_run
    'use Test; class Bar { has $.y; method raku { "MyBar(" ~ $.y ~ ")" } };'
    ~ 'is-eqv Bar.new(y=>1), Bar.new(y=>2);',
    {
        :out(/'expected: MyBar(2)' .+ 'got: MyBar(1)'/),
        :1status,
    },
    'is-eqv diagnostic honors a user-defined .raku override';
