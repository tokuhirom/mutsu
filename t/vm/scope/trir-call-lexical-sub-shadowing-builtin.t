use Test;

# A lexical `my sub` named like a core routine (`parse-base`, `chars`) shadows
# it for every call in its scope, including a call made from a routine the
# typed IR (TRIR) admits. That call used to go through the built-in funnel,
# which answered the core routine: MCP::Server::Tool::Web's
# `resolve-url(... --> Str) { resolve-ref($href, parse-base($base)) }` reached
# the core `parse-base` and died with "Cannot convert string to number".

plan 6;

{
my sub parse-base(Str $base) { "mine:" ~ $base }
my sub chars($x) { "my-chars:" ~ $x }

my sub typed-caller(Str $b --> Str) { parse-base($b) }
my sub untyped-caller(Str $b) { parse-base($b) }
my sub chars-caller($x --> Str) { chars($x) }

is typed-caller("x"), "mine:x", 'a --> Str routine calls the lexical sub, not the core routine';
is untyped-caller("y"), "mine:y", 'an untyped routine calls the lexical sub';
is chars-caller("abc"), "my-chars:abc", 'a lexical chars() shadows the core one too';

# Repeated calls keep resolving to the lexical sub.
is (^3).map({ typed-caller("r$_") }).join(","), "mine:r0,mine:r1,mine:r2",
    'every call of the site reaches the lexical sub';
}

# Outside the block that declares them, the same names reach the core
# routines again.
my sub core-caller(Str $s --> Int) { parse-base($s, 16) }
is core-caller("ff"), 255, 'an unshadowed core routine is still called';

# The same inside a package.
module M {
    my sub parse-base(Str $base) { "pkg:" ~ $base }
    our sub go(Str $b --> Str) { parse-base($b) }
}
is M::go("z"), "pkg:z", 'a package-scoped lexical sub shadows the core routine';
