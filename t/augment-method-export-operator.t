use Test;
use MONKEY-TYPING;

# ADR-0019 D3-6: `is export` on an augment-declared operator method now
# registers an importable sub form, matching the class walker. Verified
# against raku. (A plain, non-operator method name's `is export` is a
# separate, pre-existing bug shared by every walker; see
# todo/tickets/method-is-export-non-operator-name-does-nothing.md.)

plan 1;

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
