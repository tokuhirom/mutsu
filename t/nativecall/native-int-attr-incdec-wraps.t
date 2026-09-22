use Test;

# #8985: a narrow native-int attribute's `$!attr++`/`$.attr++` did not wrap on
# overflow. A same-width `my int8 $x` lexical, or a `my int8 @a` array
# element, both wrapped correctly already -- only the attribute-cell
# increment/decrement path was wrong, because it consulted a routine-scoped
# `__mutsu_type::` env entry (which a `my` declaration registers in its own
# frame) instead of the class registry's attribute type, which is where a
# `has int8 $.v` attribute's declared type actually lives. Plain assignment
# (`$!v = ...`) already fell back to the class registry and wrapped
# correctly; only the bare `++`/`--` read-modify-write tail was missing that
# fallback.

plan 8;

class PrivBump {
    has int8 $!v = 127;
    method bump { $!v++ }
    method get { $!v }
}
my $pb = PrivBump.new;
$pb.bump;
is $pb.get, -128, 'private int8 attribute post-increment wraps on overflow';

class PrivPre {
    has int8 $!v = 127;
    method bump { ++$!v }
    method get { $!v }
}
my $pp = PrivPre.new;
$pp.bump;
is $pp.get, -128, 'private int8 attribute pre-increment wraps on overflow';

class PrivDown {
    has int8 $!v = -128;
    method bump { $!v-- }
    method get { $!v }
}
my $pd = PrivDown.new;
$pd.bump;
is $pd.get, 127, 'private int8 attribute post-decrement wraps on underflow';

class PubBump {
    has int8 $.v is rw = 127;
    method bump { $.v++ }
}
my $pub = PubBump.new;
$pub.bump;
is $pub.v, -128, 'public rw int8 accessor post-increment wraps on overflow';

class Uns {
    has uint8 $!v = 250;
    method bump { $!v += 10 }
    method get { $!v }
}
my $u = Uns.new;
$u.bump;
is $u.get, 4, 'private uint8 attribute compound-add wraps modulo 256';

# A same-width lexical and array element must still wrap (pre-existing
# behavior, pinned so a fix here cannot regress them).
my int8 $lex = 127;
$lex++;
is $lex, -128, 'lexical int8 post-increment still wraps';

my int8 @arr = 127;
@arr[0]++;
is @arr[0], -128, 'int8 array element post-increment still wraps';

# An UNtyped attribute must not spuriously wrap or type-check.
class Untyped {
    has $.v = 127;
    method bump { $!v++ }
}
my $un = Untyped.new;
$un.bump;
is $un.v, 128, 'untyped attribute post-increment does not wrap';
