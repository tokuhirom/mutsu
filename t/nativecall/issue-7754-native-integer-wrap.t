use Test;

plan 40;

# In-place arithmetic on a native scalar uses the native machine operation.
{
    my int8 $a = 127;
    is ++$a, -128, 'int8 prefix increment wraps';
    is $a, -128, 'int8 increment stores the wrapped value';

    my int16 $b = -32768;
    is --$b, 32767, 'int16 prefix decrement wraps';
    is $b, 32767, 'int16 decrement stores the wrapped value';

    my int32 $c = 2147483647;
    is $c + 1, 2147483648, 'int32 arithmetic keeps machine width';
    $c += 1;
    is $c, -2147483648, 'int32 compound assignment narrows on store';

    my int $d = 9223372036854775807;
    is ++$d, -9223372036854775808, 'int prefix increment wraps at i64';
    is $d, -9223372036854775808, 'int increment stores the wrapped value';

    my int $e = -9223372036854775808;
    --$e;
    is $e, 9223372036854775807, 'int prefix decrement wraps at i64';

    my int $f = 9223372036854775807;
    $f += 1;
    is $f, -9223372036854775808, 'int + assignment uses native overflow';

    my int $g = -9223372036854775808;
    $g -= 1;
    is $g, 9223372036854775807, 'int - assignment uses native underflow';

    my int $h = 3037000500;
    $h *= $h;
    is $h, -9223372036709301616, 'int multiplication wraps at i64';
}

# Unsigned native operands use u64 arithmetic, while narrow declarations still
# narrow their result when it is written back to the variable.
{
    my uint8 $a = 255;
    ++$a;
    is $a, 0, 'uint8 prefix increment wraps';

    my uint16 $b = 0;
    --$b;
    is $b, 65535, 'uint16 prefix decrement wraps';

    my uint32 $c = 4294967295;
    $c += 1;
    is $c, 0, 'uint32 compound addition narrows on store';

    my uint $d = 18446744073709551615;
    my uint $one = 1;
    $d += $one;
    is $d, 0, 'uint addition wraps at u64';

    my uint $e = 0;
    my uint $one2 = 1;
    $e -= $one2;
    is $e, 18446744073709551615, 'uint subtraction wraps at u64';

    my uint $f = 4294967296;
    my uint $two = 4294967296;
    $f *= $two;
    is $f, 0, 'uint multiplication wraps at u64';
}

# Element stores use the same native-width rule as scalar stores.
{
    my int8 @a = 127;
    ++@a[0];
    is @a[0], -128, 'int8 array increment wraps';

    my int16 @b = -32768;
    --@b[0];
    is @b[0], 32767, 'int16 array decrement wraps';

    my int32 @c = 2147483647;
    ++@c[0];
    is @c[0], -2147483648, 'int32 array increment wraps';

    my int @d = 9223372036854775807;
    ++@d[0];
    is @d[0], -9223372036854775808, 'int array increment wraps';

    my uint8 @e = 255;
    ++@e[0];
    is @e[0], 0, 'uint8 array increment wraps';

    my uint16 @f = 0;
    --@f[0];
    is @f[0], 65535, 'uint16 array decrement wraps';

    my uint32 @g = 4294967295;
    ++@g[0];
    is @g[0], 0, 'uint32 array increment wraps';

    my uint @h = 18446744073709551615;
    ++@h[0];
    is @h[0], 0, 'uint array increment wraps';
}

# The remaining scalar boundaries cover compound subtraction, addition, and
# multiplication for the narrow signed and unsigned widths, plus direct
# full-width arithmetic and the boxed-Int control.
{
    my int8 $a = -128;
    $a -= 1;
    is $a, 127, 'int8 compound subtraction wraps';

    my int16 $b = 32767;
    $b += 1;
    is $b, -32768, 'int16 compound addition wraps';

    my int32 $c = -2147483648;
    $c -= 1;
    is $c, 2147483647, 'int32 compound subtraction wraps';

    my int8 $d = 64;
    $d *= 2;
    is $d, -128, 'int8 compound multiplication wraps';

    my uint8 $e = 0;
    $e -= 1;
    is $e, 255, 'uint8 compound subtraction wraps';

    my uint16 $f = 65535;
    $f += 1;
    is $f, 0, 'uint16 compound addition wraps';

    my uint32 $g = 65536;
    $g *= 65536;
    is $g, 0, 'uint32 compound multiplication wraps';

    my uint8 $h = 128;
    $h *= 2;
    is $h, 0, 'uint8 compound multiplication wraps';

    my int $signed_one = 1;
    my int $i = 9223372036854775807;
    is $i + $signed_one, -9223372036854775808, 'direct int addition wraps';

    my int $j = -9223372036854775808;
    is $j - $signed_one, 9223372036854775807, 'direct int subtraction wraps';

    my int $k = 3037000500;
    is $k * $k, -9223372036709301616, 'direct int multiplication wraps';

    my uint $unsigned_zero = 0;
    my uint $unsigned_one = 1;
    is $unsigned_zero - $unsigned_one, -1, 'direct uint subtraction keeps register bits';

    my Int $boxed = 9223372036854775807;
    is $boxed + 1, 9223372036854775808, 'boxed Int addition still promotes';
    is $boxed * 2, 18446744073709551614, 'boxed Int multiplication still promotes';
}
