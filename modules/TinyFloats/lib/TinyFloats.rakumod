unit class TinyFloats:auth<zef:japhb>:api<0>:ver<0.0.5>;


# In all of the following routines $num is a native float, represented during
# processing as a uint32 formatted as IEEE 754 binary32.


sub bin16-from-num($num) is export {
    my $uint = buf8.write-num32(0, $num).read-uint32(0);
    my $sign = ($uint +& 0x80000000) +> 16;
    my $exp  =  $uint +& 0x7F800000;

    # Below bin16 subnormal; drop to (signed) zero
    if    $exp < 0x33800000 {  # 2 ** -24
        $sign
    }
    # Below bin16 normal; drop to subnormal
    elsif $exp < 0x38800000 {  # 2 ** -14
        my $mant = ($uint +& 0x7FE000) +| 0x800000;
        ($mant +> (23 - (($exp - 0x33800000) +> 23))) +| $sign
    }
    # Normal range; bitpack
    elsif $exp < 0x47800000 {
        ((($exp - 0x38000000) +| ($uint +& 0x7FE000)) +> 13) +| $sign
    }
    # 32-bit Inf or NaN
    elsif $exp == 0x7F800000 {
        my $inf-nan = (($uint +& 0x7FE000) +> 13) +| 0x7C00;
        # Use sign bit for Inf only, not NaN due to Windows incompatibility
        ($inf-nan +& 0x3FF) ?? $inf-nan !! $inf-nan +| $sign
    }
    # 16-bit Inf
    else {
        0x7C00 +| $sign
    }
}

sub num-from-bin16($bin16) is export {
    # Algorithm:
    # * Separate the sign bit
    # * If exponent is non-zero:
    #   * Shift exponent and mantissa into proper bit position
    #   * Rebias exponent
    #   * Fix up Inf/NaN bit patterns
    #   * Bring back sign bit, shifted into proper place
    #   * Convert from bit pattern to native num
    # * Else if exponent is zero:
    #   * Absolute value is mantissa divided by 1024e0
    #   * Use correct sign

    my $sign = $bin16 +& 0x8000;

    # Non-zero exponent: Normal, NaN, or Inf
    if $bin16 +& 0x7C00 {
        my $rest = ($bin16 +& 0x7FFF) +< 13 + 0x38000000;
        $rest = $rest +| 0x38000000 if $rest >= 0x47800000;
        buf8.write-uint32(0, $rest +| ($sign +< 16)).read-num32(0)
    }
    # Zero exponent: Zero or subnormal
    else {
        my $abs = ($bin16 +& 0x03FF) / 16777216e0;
        $sign ?? -$abs !! $abs
    }
}


sub tf32-from-num($num) is export {
    # tf32 is simply IEEE 754 binary32 with the bottom 13 bits chopped off
    my $tf32 = buf8.write-num32(0, $num).read-uint32(0) +> 13;
    # Force NaN sign bit off due to Windows incompatibility
    $tf32 == 0x7FE00 ?? 0x3FE00 !! $tf32
}

sub num-from-tf32($tf32) is export {
    # tf32 is simply IEEE 754 binary32 with the bottom 13 bits chopped off
    buf8.write-uint32(0, $tf32 +< 13).read-num32(0)
}


sub bf16-from-num($num) is export {
    # bf16 is simply IEEE 754 binary32 with the bottom 16 bits chopped off
    my $bf16 = buf8.write-num32(0, $num).read-uint32(0) +> 16;
    # Force NaN sign bit off due to Windows incompatibility
    $bf16 == 0xFFC0 ?? 0x7FC0 !! $bf16
}

sub num-from-bf16($bf16) is export {
    # bf16 is simply IEEE 754 binary32 with the bottom 16 bits chopped off
    buf8.write-uint32(0, $bf16 +< 16).read-num32(0)
}


sub e5m2-from-num($num) is export {
    # e5m2 is simply bin16 with the bottom 8 bits chopped off
    bin16-from-num($num) +> 8
}

sub num-from-e5m2($e5m2) is export {
    # e5m2 is simply bin16 with the bottom 8 bits chopped off
    num-from-bin16($e5m2 +< 8)
}


=begin pod

=head1 NAME

TinyFloats - Convert to/from tiny float formats

=head1 SYNOPSIS

=begin code :lang<raku>

use TinyFloats;

my $tf32  = tf32-from-num(1e0);      # 0x5F800
my $num1  = num-from-tf32(0x7FBFF);  # -3.4011621342146535e+38

my $bf16  = bf16-from-num(1e0);      # 0x3F80
my $num2  = num-from-bf16(0xFF7F);   # -3.3895313892515355e+38

my $bin16 = bin16-from-num(1e0);     # 0x3C00
my $num3  = num-from-bin16(0xFBFF);  # -65504e0

my $e5m2  = e5m2-from-num(1e0);      # 0x3C
my $num4  = num-from-e5m2(0xFB);     # -57344e0

=end code

=head1 DESCRIPTION

TinyFloats is a collection of simple conversion routines to help with storing
floating point data in tiny float formats.  Raku B<cannot> compute with these
shorter formats directly; they must first be converted back to native floating
point using one of the C<num-from-*> routines.

This version supports I<only> bidirectional conversion between Raku native
floating point numbers (C<num>/C<num32>/C<num64>) and the following shorter
floating point storage formats from this table:

=begin table :caption<Formats and Bit Widths>

Name  | Total | Exponent | Mantissa | Max Val | ±Inf? | NaN? | Notes
======================================================================================
num64 |    64 |       11 |       52 | ~2e+308 |   Y   |  Y   | Raku native (= num)
num32 |    32 |        8 |       23 |  ~3e+38 |   Y   |  Y   | Raku native
tf32  |    19 |        8 |       10 |  ~3e+38 |   Y   |  Y   | Nvidia TPU internal
bf16  |    16 |        8 |        7 |  ~3e+38 |   Y   |  Y   | bfloat16, truncated num32
bin16 |    16 |        5 |       10 |   65504 |   Y   |  Y   | IEEE 754 binary16/half
e5m2  |     8 |        5 |        2 |   57344 |   Y   |  Y   | FP8, truncated bin16

=end table

More details on the supported formats:

=item C<num64>/C<num>

IEEE 754 C<binary64> ("double precision") format, AKA C<double> in C, C<float64>
in CDDL, and C<7.27> in CBOR.  Natively handled in Raku; unless specified
otherwise, all floating point computation in Raku is done in this format.

=item C<num32>

IEEE 754 C<binary32> ("single precision") format, AKA C<float> in C, C<float32>
in CDDL, and C<7.26> in CBOR.  Natively handled in Raku, but used in Raku
computations only if specifically requested.  This is also used as an
intermediate format when expanding the shorter formats using one of the
C<num-from-*> routines.

=item C<tf32>

Nvidia TensorFloat-32 format, used internally by Nvidia tensor processing
hardware, with the 8 bit exponent width of C<num32> and C<bf16> and the 10 bit
mantissa width of C<bin16>.  This works well for improving performance without
as much loss of range or precision as the 16-bit formats.  Unfortunately it is
a 19-bit format and thus inconvenient to use for storage or interchange; Nvidia
TPUs convert from C<num32> and back on the fly.  This format is provided here
for completeness, but is generally just a curiosity if not performing
computations on actual Nvidia TPU hardware.

=item C<bf16>

Google Brain C<bfloat16> format, essentially IEEE 754 C<binary32> ("single
precision") with the least significant 16 bits truncated from the mantissa.
C<bf16> reduces I<only> the mantissa bits, is relatively quick to convert, is
used most often in machine learning systems, and is usually supported directly
by the ML hardware.

=item C<bin16>

IEEE 754 C<binary16> ("half precision") format, AKA C<_Float16> in C,
C<float16> in CDDL, and C<7.25> in CBOR.  C<bin16> attempts to balance the
reduction in exponent and mantissa bits, is fairly slow to convert, is used
most often in graphics formats, and is commonly converted to native binary32
internally by modern graphics hardware.

=item C<e5m2>

Open Compute Project (OCP) OFP8 format, E5M2 variant.  Another truncated
format, C<e5m2> is C<bin16> with the least significant 8 bits truncated from
the mantissa.  Like C<bf16>, C<e5m2> reduces I<only> the mantissa bits from its
parent format, and thus maintains most of its available useful range.  Unlike
the other OFP8 variant (E4M3), C<e5m2> still maintains full C<Inf>/C<NaN>
support.

=head1 BUGS AND LIMITATIONS

=item There are no routines provided to directly convert between various tiny float formats.  To do this, you will need to convert from the source tiny format to a native Raku C<num> using the appropriate C<num-from-*> routine before converting to the destination tiny format using the appropriate C<*-from-num> routine.

=item Currently only mantissa truncation (AKA RTZ, Round To Zero) is supported when converting to narrower formats.  Recent standardization work requires that format conversions support and default to using IEEE 754 RTNE (Round To Nearest with ties to Even) instead, though truncation is allowed as an option.  For now the routines in this module comply with the older and faster truncation-focused rounding.

=item There are no routines provided to pack sub-byte formats into a byte buffer (C<buf8>); callers will need to do this themselves for now.

=head1 AUTHOR

Geoffrey Broadwell <gjb@sonic.net>

=head1 COPYRIGHT AND LICENSE

Copyright 2021,2025 Geoffrey Broadwell

This library is free software; you can redistribute it and/or modify it under
the Artistic License 2.0.

=end pod
