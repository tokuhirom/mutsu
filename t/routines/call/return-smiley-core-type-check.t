use Test;

# Perf fast paths for core types with a definiteness smiley (`--> Bool:D`,
# `--> Int:D`) must keep the semantics (from MCP::Server::Tool::Web's helpers).

plan 8;

my sub is-space(Str $ch --> Bool:D) { $ch eq ' ' }
my sub five($x --> Int:D) { 5 }
my sub undef-int($x --> Int:D) { Int }
my sub wrong($x --> Int:D) { "a" }
my sub u($x --> Int:U) { Int }

ok is-space(' '), "Bool:D return";
nok is-space('x'), "Bool:D return false";
is five(1), 5, "Int:D return";
dies-ok { undef-int(1) }, "Int:D rejects the type object";
dies-ok { wrong(1) }, "Int:D rejects a Str";
lives-ok { u(1) }, "Int:U accepts the type object";
my $n = 0;
$n++ if is-space(' ') for ^100;
is $n, 100, "repeated calls";
is (sub ($x --> Str:D) { "ok" }).(1), "ok", "anonymous sub Str:D";
