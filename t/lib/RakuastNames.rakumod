unit module RakuastNames;

# A fixture for t/rakuast/rakuast-names-and-literals.t: a type, a constant, an
# enum and a routine a unit can import.

class RakuastNamesCls is export { method hi { 'hi' } }
our constant RAKUAST-K is export = 3;
enum RakuastEnum is export <rn-a rn-b>;
sub rakuast-names-sub is export { 7 }
