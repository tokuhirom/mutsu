use Test;

# XML::Class t/090: `class Test::Bool` at file scope must not become the `Bool`
# that code inside the Test module (`done-testing --> Bool:D`) resolves.
plan 2;

class Test::Bool { has Bool $.attribute; }

ok Test::Bool.new(attribute => True).attribute, "class with a Bool attribute works";
is (done-testing).^name, "Bool", "done-testing still returns the core Bool";
