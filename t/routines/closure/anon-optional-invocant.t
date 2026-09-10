use v6;
use Test;

plan 5;

# `$?` (and `@?` / `%?`) is a valid *anonymous optional parameter*, including in
# invocant position: `method m($?: |c) { ... }` names no invocant but still binds
# self. mutsu recognized `$?` as anonymous only before `,` / `)` / whitespace, so
# `$?` followed by the invocant `:` fell through to scalar-var parsing and threw
# "X::Syntax::Perl5Var: Unsupported use of $? variable". Seen in the
# PDF::Font::Loader::CSS dist: `multi method find-font($?: |) { nextsame }`.

class C {
    method tag($?: |c) { "tag:" ~ c.list.join(",") }
}

is C.tag(1, 2, 3), 'tag:1,2,3', 'anonymous-invocant method binds self and captures args';
is C.new.tag("a"), 'tag:a',     'anonymous-invocant method works on an instance too';

# The plain (non-invocant) anonymous optional param still works.
sub f($?) { "ok" }
is f(),  'ok', 'anonymous optional param accepts zero args';
is f(1), 'ok', 'anonymous optional param accepts one arg';

# A bare `$?` term is still the unsupported Perl5 variable.
throws-like { EVAL 'my $? = 5' }, X::Syntax::Perl5Var,
    'bare $? term is still rejected as a Perl5 variable';
