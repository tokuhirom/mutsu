[![Actions Status](https://github.com/japhb/TinyFloats/workflows/test/badge.svg)](https://github.com/japhb/TinyFloats/actions)

NAME
====

TinyFloats - Convert to/from tiny float formats

SYNOPSIS
========

```raku
use TinyFloats;

my $tf32  = tf32-from-num(1e0);      # 0x5F800
my $num1  = num-from-tf32(0x7FBFF);  # -3.4011621342146535e+38

my $bf16  = bf16-from-num(1e0);      # 0x3F80
my $num2  = num-from-bf16(0xFF7F);   # -3.3895313892515355e+38

my $bin16 = bin16-from-num(1e0);     # 0x3C00
my $num3  = num-from-bin16(0xFBFF);  # -65504e0

my $e5m2  = e5m2-from-num(1e0);      # 0x3C
my $num4  = num-from-e5m2(0xFB);     # -57344e0
```

DESCRIPTION
===========

TinyFloats is a collection of simple conversion routines to help with storing floating point data in tiny float formats. Raku **cannot** compute with these shorter formats directly; they must first be converted back to native floating point using one of the `num-from-*` routines.

This version supports *only* bidirectional conversion between Raku native floating point numbers (`num`/`num32`/`num64`) and the following shorter floating point storage formats from this table:

<table class="pod-table">
<caption>Formats and Bit Widths</caption>
<thead><tr>
<th>Name</th> <th>Total</th> <th>Exponent</th> <th>Mantissa</th> <th>Max Val</th> <th>±Inf?</th> <th>NaN?</th> <th>Notes</th>
</tr></thead>
<tbody>
<tr> <td>num64</td> <td>64</td> <td>11</td> <td>52</td> <td>~2e+308</td> <td>Y</td> <td>Y</td> <td>Raku native (= num)</td> </tr> <tr> <td>num32</td> <td>32</td> <td>8</td> <td>23</td> <td>~3e+38</td> <td>Y</td> <td>Y</td> <td>Raku native</td> </tr> <tr> <td>tf32</td> <td>19</td> <td>8</td> <td>10</td> <td>~3e+38</td> <td>Y</td> <td>Y</td> <td>Nvidia TPU internal</td> </tr> <tr> <td>bf16</td> <td>16</td> <td>8</td> <td>7</td> <td>~3e+38</td> <td>Y</td> <td>Y</td> <td>bfloat16, truncated num32</td> </tr> <tr> <td>bin16</td> <td>16</td> <td>5</td> <td>10</td> <td>65504</td> <td>Y</td> <td>Y</td> <td>IEEE 754 binary16/half</td> </tr> <tr> <td>e5m2</td> <td>8</td> <td>5</td> <td>2</td> <td>57344</td> <td>Y</td> <td>Y</td> <td>FP8, truncated bin16</td> </tr>
</tbody>
</table>

More details on the supported formats:

  * `num64`/`num`

IEEE 754 `binary64` ("double precision") format, AKA `double` in C, `float64` in CDDL, and `7.27` in CBOR. Natively handled in Raku; unless specified otherwise, all floating point computation in Raku is done in this format.

  * `num32`

IEEE 754 `binary32` ("single precision") format, AKA `float` in C, `float32` in CDDL, and `7.26` in CBOR. Natively handled in Raku, but used in Raku computations only if specifically requested. This is also used as an intermediate format when expanding the shorter formats using one of the `num-from-*` routines.

  * `tf32`

Nvidia TensorFloat-32 format, used internally by Nvidia tensor processing hardware, with the 8 bit exponent width of `num32` and `bf16` and the 10 bit mantissa width of `bin16`. This works well for improving performance without as much loss of range or precision as the 16-bit formats. Unfortunately it is a 19-bit format and thus inconvenient to use for storage or interchange; Nvidia TPUs convert from `num32` and back on the fly. This format is provided here for completeness, but is generally just a curiosity if not performing computations on actual Nvidia TPU hardware.

  * `bf16`

Google Brain `bfloat16` format, essentially IEEE 754 `binary32` ("single precision") with the least significant 16 bits truncated from the mantissa. `bf16` reduces *only* the mantissa bits, is relatively quick to convert, is used most often in machine learning systems, and is usually supported directly by the ML hardware.

  * `bin16`

IEEE 754 `binary16` ("half precision") format, AKA `_Float16` in C, `float16` in CDDL, and `7.25` in CBOR. `bin16` attempts to balance the reduction in exponent and mantissa bits, is fairly slow to convert, is used most often in graphics formats, and is commonly converted to native binary32 internally by modern graphics hardware.

  * `e5m2`

Open Compute Project (OCP) OFP8 format, E5M2 variant. Another truncated format, `e5m2` is `bin16` with the least significant 8 bits truncated from the mantissa. Like `bf16`, `e5m2` reduces *only* the mantissa bits from its parent format, and thus maintains most of its available useful range. Unlike the other OFP8 variant (E4M3), `e5m2` still maintains full `Inf`/`NaN` support.

BUGS AND LIMITATIONS
====================

  * There are no routines provided to directly convert between various tiny float formats. To do this, you will need to convert from the source tiny format to a native Raku `num` using the appropriate `num-from-*` routine before converting to the destination tiny format using the appropriate `*-from-num` routine.

  * Currently only mantissa truncation (AKA RTZ, Round To Zero) is supported when converting to narrower formats. Recent standardization work requires that format conversions support and default to using IEEE 754 RTNE (Round To Nearest with ties to Even) instead, though truncation is allowed as an option. For now the routines in this module comply with the older and faster truncation-focused rounding.

  * There are no routines provided to pack sub-byte formats into a byte buffer (`buf8`); callers will need to do this themselves for now.

AUTHOR
======

Geoffrey Broadwell <gjb@sonic.net>

COPYRIGHT AND LICENSE
=====================

Copyright 2021,2025 Geoffrey Broadwell

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

