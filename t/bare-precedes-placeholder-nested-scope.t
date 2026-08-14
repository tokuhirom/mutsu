use Test;

plan 9;

# A placeholder parameter ($^b) declares its block's $b under the *plain*
# name, so a bare $b written before the $^b that declares it is
# X::Undeclared (or X::Placeholder::NonPlaceholder if $b already exists in an
# outer scope). See news/2026-08/bare-precedes-placeholder-nested-scope.md.

# --- Same-scope ordering (pre-existing behaviour; pinned here since it had
# no dedicated test file before) ---

eval-dies-ok 'my $f = { say $b; $^b }; $f(42)',
    'bare $b before $^b in the same block is X::Undeclared';

eval-dies-ok 'my $b = "outer"; my $f = { say $b; $^b }; $f(42)',
    'bare $b before $^b, with $b already in an outer scope, still dies (X::Placeholder::NonPlaceholder)';

# --- Nested-block scope: the two repro cases from the ticket. A $^b used
# ONLY inside a nested if/for BLOCK body belongs to that inner block, not the
# enclosing one, so a bare $b in the enclosing block was never declared. ---

eval-dies-ok 'my $f = { for 1 { $^b }; say $b }; $f(42)',
    '$^b inside a nested `for` BLOCK does not declare the outer $b (X::Undeclared)';

eval-dies-ok 'my $f = { if 1 { $^b }; say $b }; $f(42)',
    '$^b inside a nested `if` BLOCK does not declare the outer $b (X::Undeclared)';

eval-dies-ok 'my $f = { given 5 { $^b }; say $b }; $f(42)',
    '$^b inside a nested `given` BLOCK does not declare the outer $b (X::Undeclared)';

# --- The `for` statement MODIFIER is not a block: its body runs in the
# enclosing scope, so $^b there DOES declare the enclosing block's $b. This
# must keep working (see news/2026-08/for-modifier-placeholder-scope.md). ---

lives-ok '{ say $^b for 1; say $b }',
    '$^b in a `for` statement MODIFIER declares the enclosing block\'s $b (stays legal)';

# --- A bare $name that IS otherwise declared (locally, or in an outer
# scope) is unaffected by an unrelated nested placeholder of the same name. ---

lives-ok 'my $f = { my $b; for 1 { $^b }; say $b }; $f(42)',
    'a bare $b that is `my`-declared locally is unaffected by a nested $^b';

lives-ok 'my $b = 99; my $f = { for 1 { $^b }; say $b }; $f(42)',
    'a bare $b declared in an outer scope is unaffected by a nested $^b';

# --- A placeholder that belongs to a genuinely separate closure (not
# reachable via the enclosing block's own placeholder scope) still shadows a
# same-named bare use in the enclosing block once referenced there. ---

eval-dies-ok 'my $g = { my $h = { $^c }; say $c }; $g()',
    '$^c inside a nested closure does not declare the outer $c (X::Undeclared)';
