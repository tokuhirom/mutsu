use Test;

# Rakudo declares no `zprintf` method on `Str` or `Cool` (ADR-11276 §9.104):
# the `zprintf` routine is a mutsu extension, and its method form is gone.

plan 3;

throws-like { '%d'.zprintf(1) }, X::Method::NotFound, 'Str has no zprintf method';
throws-like { 42.zprintf('x') }, X::Method::NotFound, 'nor does Cool';
is zprintf('%d', 7), '7', 'the routine form stays';
