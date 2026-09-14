use Test;

# A user-defined `prefix:<->` must not consume the leading `-` of `->`.
# Red::Operators declares this prefix and then uses pointy lambdas in method
# arguments, such as `$b.map(-> $v { ... })`; mutsu used to report a parse error
# at the lambda arrow instead of loading the module.

plan 2;

multi prefix:<->(Any $value) is export { $value };

is-deeply (1, 2).map(-> $v { $v + 1 }).List, (2, 3),
    'a declared minus prefix does not shadow a pointy lambda';
is-deeply (1, 2).map(-> $v { $v ~~ Enumeration ?? $v.value !! $v }).List, (1, 2),
    'the Red::Operators lambda shape remains usable';
