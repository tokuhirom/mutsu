use Test;
use MONKEY-TYPING;

# ADR-0019 D3-6: `is export` on an augment-declared method registers an
# importable sub form, matching the class walker. Verified against raku.
# Covers both an operator-categorical name and a plain one -- the plain-name
# case was a separate, pre-existing bug shared by every walker (fixed
# alongside this; see news/2026-08/method-is-export-non-operator-name.md).

plan 2;

class Exported1 {
    has $.val;
    method Str() { "Exported1(" ~ $!val ~ ")" }
}
augment class Exported1 {
    method infix:<as-str>($other) is export { self.Str ~ '+' ~ $other.Str }
}
import Exported1;
my $a = Exported1.new(val => 1);
my $b = Exported1.new(val => 2);
is ($a as-str $b), 'Exported1(1)+Exported1(2)',
    'is export on an augment-declared operator method registers an importable sub';

class Exported2 { }
augment class Exported2 {
    method greet() is export { "hi" }
}
import Exported2;
is greet(Exported2.new), 'hi',
    'is export on an augment-declared plain-named method registers an importable sub';
