use Test;

# `augment class BuiltinType does Role { }` (MONKEY-TYPING) must mix the role's
# methods into the builtin type, and an augmented method must override the
# native one (Rakudo semantics). Previously the `does Role` clause on the
# augment declaration was mis-parsed (`augment` swallowed as a bareword, the
# rest parsed as a plain `class Str does Role` redeclaration), so the role's
# methods were never composed onto the type.

plan 5;

role Rotate {
    method rotate-str (Int $n = 1) {
        my $shft = abs($n % self.chars);
        self.substr($shft) ~ self.substr(0, $shft)
    }
}

use MONKEY-TYPING;
augment class Str does Rotate { }

is 'Rakudo'.rotate-str,     'akudoR', 'augmented role method (default arg)';
is 'Rakudo'.rotate-str(-1), 'oRakud', 'augmented role method (explicit arg)';
ok 'Rakudo'.^can('rotate-str'), '.^can reports the composed method';

# A method whose name collides with a native list method that hard-errors on a
# Str (`.rotate` is List-only in Rakudo) must dispatch to the augmented role
# method, not raise "No such method".
role Twist {
    method rotate (Int $n = 1) { "twist:$n" }
}
augment class Str does Twist { }
is 'x'.rotate,   'twist:1', 'augmented method overrides native .rotate';
is 'x'.rotate(4), 'twist:4', 'augmented method overrides native .rotate (arg)';
