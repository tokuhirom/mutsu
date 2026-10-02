use lib 't/lib';
use Test;
use ExportedMultiLexicalRole;

# A `my role` / `my class` is stored under a declaration-site key, and only its
# declaring scope's env maps the source spelling to it. A `multi` exported from
# such a module is matched in the CALLER's env, where `Tagged` names nothing,
# so a role-typed candidate never matched ("Cannot resolve caller ...").
# Reduced from the Zef distribution P5print, whose
# `multi sub print(P5Handle $handle, *@_) is export` takes `$*OUT but P5Handle`.

plan 5;

is describe(tagged-handle()), 'tagged', 'exported multi accepts an object mixed with a my-role';
is describe(plain()), 'plain', 'exported multi accepts an instance of a my-class';
is describe("x"), 'str', 'unrelated candidate still wins for its own type';
is describe(tagged-int()), 'tagged', 'a value mixin also matches the role candidate';
my $n = 42;
dies-ok { describe($n) }, 'an argument matching no candidate is still rejected';
