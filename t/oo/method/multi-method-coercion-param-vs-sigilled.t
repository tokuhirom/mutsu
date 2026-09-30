use Test;

# Came from Config::Parser::toml (Config.read): a `IO() $path` multi method
# must not out-rank an unconstrained `%data` / `@data` candidate for a
# Hash / Array argument, since `IO()` accepts `Any` and `%`/`@` imply
# Associative/Positional.

plan 4;

class C {
    multi method a(IO() $path) { "io" }
    multi method a(%data) { "hash" }
    multi method b(Str() $s) { "str" }
    multi method b(%data) { "hash" }
    multi method d(IO() $path) { "io" }
    multi method d(@data) { "arr" }
}

is C.new.a(%(a => 1)), "hash", "IO() vs %data, Hash arg";
is C.new.b(%(a => 1)), "hash", "Str() vs %data, Hash arg";
is C.new.d([1]), "arr", "IO() vs @data, Array arg";
is C.new.a("x"), "io", "IO() still chosen for a Str arg";
