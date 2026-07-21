use Test;

plan 6;

# An attribute declaration is a complete statement. A bare term after the
# attribute (with no `;`/`}` between) is "two terms in a row", exactly as raku
# reports it (X::Syntax::Confused) — NOT a fresh statement. Previously mutsu
# silently started a new statement, so `has $.a syntax error` parsed the trailing
# `syntax error` as an undeclared listop call instead of a syntax error.
throws-like 'my class A { has $.a syntax error; }', X::Syntax::Confused,
    'a bare term after an attribute is Confused, not a new statement';

throws-like "my class B \{ has \$.a\n has \$.b\n }", X::Syntax::Confused,
    'two attribute declarations without a separator are Confused';

# Well-formed attribute declarations keep parsing.
lives-ok { EVAL 'my class C { has $.a is rw; has $.b = 5 }' },
    'properly separated attributes still parse';

lives-ok { EVAL 'my class D { has Int $.a where * > 0 = 3 }' },
    'type / where / default chain still parses';

lives-ok { EVAL 'my class E { has $.a of Int = 3 }' },
    'of / default chain still parses';

# A block/closure default ends in `}`, which implicitly terminates the
# statement in Raku, so a following declaration on the next line (no `;`) is a
# new statement — NOT a second term. This must not be rejected as Confused.
lives-ok { EVAL 'my class F { has $.cl = { self.foo }
        method foo { 42 } }' },
    'a block-terminated default allows a following declaration without a semicolon';
