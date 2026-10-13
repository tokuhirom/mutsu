use v6.d;
use Test;

# Gnome::Glib (via Gnome::N::TopLevelClassSupport): NativeCall's setup does
# `self.package.REPR` on a class that declares FALLBACK. `.REPR` is a Mu
# pseudo-method, so it must never reach the class's FALLBACK.

plan 4;

class A { method FALLBACK($name, |c) { "fb:$name" } }
class B is A { }

is A.REPR, 'P6opaque', '.REPR on a type object with FALLBACK';
is A.new.REPR, 'P6opaque', '.REPR on an instance with FALLBACK';
is B.REPR, 'P6opaque', '.REPR on a subclass of a FALLBACK class';
is A.nope, 'fb:nope', 'FALLBACK still answers unknown methods';
