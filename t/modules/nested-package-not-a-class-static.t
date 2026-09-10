use Test;

plan 6;

# A package declared two levels deep binds its short name into the *inner*
# body's env, and a class body deliberately keeps its env on success — so the
# short name used to survive into the enclosing class body and be mistaken for
# a class-body `my` static. That made every method of the outer class switch
# `current_package` to the class, so a method-body `sub` registered under
# `Outer::` while a lazily-forced `gather` body resolved it under `GLOBAL::`
# and could not find it (File::Ignore's `method walk`).

class Outer {
    class Inner { class Deepest { } }
    method w() {
        sub r() { take 7 }
        gather r()
    }
}
is Outer.new.w.List, (7), 'method-body sub is callable from a bare gather under a doubly-nested class';

class OuterBlock {
    class Inner { class Deepest { } }
    method w() {
        sub r() { take 8 }
        gather { r() }
    }
}
is OuterBlock.new.w.List, (8), 'and from a gather block';

# The dist shape: a recursive method-body sub called from gather. Reduced, this
# was a silent empty gather; with a recursive call it died with
# "Unknown function: recurse", so pin both.
class Walker {
    class Rule { grammar Parser { }; class Compiler { } }
    method walk($n) {
        sub recurse($i) {
            return if $i > $n;
            take $i;
            recurse($i + 1);
        }
        gather recurse(1);
    }
}
is Walker.new.walk(4).List, (1, 2, 3, 4), 'a recursive method-body sub resolves inside gather';

# A role nested two deep is the same shape.
class OuterRole {
    class Inner { role Deep { } }
    method w() {
        sub r() { take 9 }
        gather r()
    }
}
is OuterRole.new.w.List, (9), 'a doubly-nested role behaves the same';

# A real class-body `my` static must still be seen by the methods.
class WithStatic {
    class Inner { class Deepest { } }
    my $counter = 41;
    method bump() { $counter++ }
}
is WithStatic.new.bump, 41, 'a genuine class-body static is still injected into methods';

# A class-body `my` holding a type object is a static too, and must not be
# dropped by the nested-package filter.
class HoldsType {
    class Inner { class Deepest { } }
    my $held = Int;
    method held() { $held }
}
is HoldsType.new.held.^name, 'Int', 'a class-body static bound to a type object survives';
