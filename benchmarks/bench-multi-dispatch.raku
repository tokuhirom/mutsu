# Multi dispatch: the call shapes whose candidate selection differs.
#
# - nominal: candidates distinguished by type alone (the winner is a pure
#   function of the argument types);
# - subset / where: a value-dependent candidate (`UInt` is a subset), whose
#   predicate has to run on every call;
# - operator + callsame + dynamic variable: FiniteField-style modular
#   arithmetic (`multi infix:<*>(UInt, UInt) { callsame() mod $*modulus }`,
#   the EC/secp256k1 hot path, #9967);
# - multi method: candidates on a class.
#
# Each section's result feeds the checksum so nothing is dead code.

my $checksum = 0;
my $t0 = now;

# --- nominal multi sub --------------------------------------------------------
multi kind(Int $x)  { 1 }
multi kind(Str $x)  { 2 }
multi kind(Rat $x)  { 3 }
multi kind(Num $x)  { 4 }
my @mixed = 1, "a", 1/2, 1e0;
for ^30000 -> $i {
    $checksum += kind(@mixed[$i % 4]);
}

# --- subset / where multi sub -------------------------------------------------
subset Small of Int where * < 100;
multi size(Small $x)                  { 1 }
multi size(Int $x where * < 10_000)   { 2 }
multi size(Int $x)                    { 3 }
for ^30000 -> $i {
    $checksum += size(($i * 37) % 20_000);
}

# --- user operator with callsame and a dynamic modulus -----------------------
{
    my $*modulus = 1_000_003;
    multi infix:<*>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    my $acc = 1;
    for 1..15000 -> $i {
        $acc = $acc * $i + $i;
    }
    $checksum += $acc;
}

# --- multi method -------------------------------------------------------------
class Shape {
    multi method scale(Int $n)       { $n * 2 }
    multi method scale(Rat $r)       { ($r * 4).Int }
    multi method scale(Str $s)       { $s.chars }
}
my $shape = Shape.new;
my @args = 3, 3/4, "abcd";
for ^30000 -> $i {
    $checksum += $shape.scale(@args[$i % 3]);
}

say "bench-section-seconds: {now - $t0}";
say "checksum = $checksum";
