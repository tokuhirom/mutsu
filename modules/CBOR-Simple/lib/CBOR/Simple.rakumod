unit module CBOR::Simple:auth<zef:japhb>:api<0>:ver<0.1.4>;

use nqp;
use TinyFloats;


enum CBORMajorType (
    CBOR_UInt  => 0,
    CBOR_NInt  => 1 +< 5,
    CBOR_BStr  => 2 +< 5,
    CBOR_TStr  => 3 +< 5,
    CBOR_Array => 4 +< 5,
    CBOR_Map   => 5 +< 5,
    CBOR_Tag   => 6 +< 5,
    CBOR_SVal  => 7 +< 5,
);


enum CBORMagicNumber (
    CBOR_MajorType_Mask => 0xE0,
    CBOR_Argument_Mask  => 0x1F,

    CBOR_False => 20,
    CBOR_True  => 21,
    CBOR_Null  => 22,
    CBOR_Undef => 23,

    CBOR_1Byte => 24,
    CBOR_2Byte => 25,
    CBOR_4Byte => 26,
    CBOR_8Byte => 27,

    CBOR_Indefinite_Break => 31,
);


enum CBORMinMax (
    CBOR_Max_UInt_1Byte => 255,
    CBOR_Max_UInt_2Byte => 65535,
    CBOR_Max_UInt_4Byte => 4294967295,
    CBOR_Max_UInt_8Byte => 18446744073709551615,
    CBOR_Max_UInt_63Bit => 9223372036854775807,

    CBOR_Min_NInt_1Byte => -256,
    CBOR_Min_NInt_2Byte => -65536,
    CBOR_Min_NInt_4Byte => -4294967296,
    CBOR_Min_NInt_8Byte => -18446744073709551616,
    CBOR_Min_NInt_63Bit => -9223372036854775808,
);


enum CBORTagNumber (
    CBOR_Tag_DateTime_String   => 0,
    CBOR_Tag_DateTime_Number   => 1,
    CBOR_Tag_Unsigned_BigInt   => 2,
    CBOR_Tag_Negative_BigInt   => 3,
    CBOR_Tag_Decimal_Fraction  => 4,
    CBOR_Tag_Bigfloat          => 5,

    #  6..15 unassigned
    # 16..18 NYI
    # 19..20 unassigned
    # 21..23 NYI

    CBOR_Tag_Encoded_CBOR      => 24,

    # 25..29 NYI

    CBOR_Tag_Rational          => 30,
    CBOR_Tag_Absent            => 31,

    # 32..34 NYI
    # 35     deprecated
    # 36..47 NYI
    # 48..60 unassigned or deprecated
    # 61     NYI
    # 62     unassigned

    CBOR_Tag_Encoded_Sequence  => 63,

    # 64..79 NYI
    # 80..87 Supported as a block
    # 88..95 unassigned
    # 96..98 NYI
    # 99     unassigned

    CBOR_Tag_Date_Integer      => 100,

    CBOR_Tag_Set               => 258,
    CBOR_Tag_Object_Key_Map    => 259,
    CBOR_Tag_Decimal_Extended  => 264,
    CBOR_Tag_Bigfloat_Extended => 265,
    CBOR_Tag_String_Key_Map    => 275,
    CBOR_Tag_Date_String       => 1004,
    CBOR_Tag_Capture           => 25441,  # This is the reference implementation!
    CBOR_Tag_Self_Described    => 55799,
    CBOR_Tag_Self_Sequence     => 55800,

    CBOR_Tag_Invalid_2Byte     => 65535,
    CBOR_Tag_Invalid_4Byte     => 4294967295,
    CBOR_Tag_Invalid_8Byte     => 18446744073709551615,
);


# Break sentinel
my class Break { }


# Introspection of tagged values
class Tagged {
    has UInt:D $.tag-number is required;
    has Mu     $.value      is required;
    has Str:D  $.desc       =  '';
}


# Parsing exceptions
class X::Malformed is X::AdHoc {}

PROCESS::<$CBOR_SIMPLE_FATAL_ERRORS> = False;


# Buffer read/write constants
my int $ne8  = nqp::bitor_i(nqp::const::BINARY_SIZE_8_BIT, NativeEndian);
my int $be16 = nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT, BigEndian);
my int $be32 = nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT, BigEndian);
my int $be64 = nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT, BigEndian);


# Precache a utf8 encoder, since we'll be doing it a LOT
my $utf8-encoder = Encoding::Registry.find("utf8").encoder;


# Don't use the RFC 8949 map key sorting, it's really slow
constant RFC8949_Map_Key_Sort = False;


# Encode an arbitrary value to CBOR, possibly with leading self-describing tag
multi cbor-encode(Mu $value, :$cbor-self-tag, :$cbor-sequence-tag) is export {
    my $pos;
    $cbor-sequence-tag
    ?? cbor-encode($value, $pos = 3, buf8.new(0xd9, 0xd9, 0xf8))
    !! $cbor-self-tag
       ?? cbor-encode($value, $pos = 3, buf8.new(0xd9, 0xd9, 0xf7))
       !! cbor-encode($value, $pos = 0)
}

# Encode an arbitrary value to CBOR, specifying a buffer position to begin writing
multi cbor-encode(Mu $value, Int:D $pos is rw, Buf:D $buf = buf8.new) is export {
    # This gets called a LOT, so go for speed
    my &write-uint = -> int $major-type, int $value {
        nqp::if(
            nqp::islt_i($value, CBOR_1Byte),
            nqp::writeuint($buf, $pos++, nqp::add_i($major-type, $value), $ne8),
            nqp::if(
                nqp::isle_i($value, CBOR_Max_UInt_1Byte),
                nqp::stmts(
                    nqp::writeuint($buf, $pos++, nqp::add_i($major-type, CBOR_1Byte), $ne8),
                    nqp::writeuint($buf, $pos++, $value, $ne8),
                ),
                nqp::if(
                    nqp::isle_i($value, CBOR_Max_UInt_2Byte),
                    nqp::stmts(
                        nqp::writeuint($buf, $pos++, nqp::add_i($major-type, CBOR_2Byte), $ne8),
                        nqp::writeuint($buf, $pos, $value, $be16),
                        ($pos = nqp::add_I(nqp::decont($pos), 2, Int)),
                    ),
                    nqp::if(
                        nqp::isle_i($value, CBOR_Max_UInt_4Byte),
                        nqp::stmts(
                            nqp::writeuint($buf, $pos++, nqp::add_i($major-type, CBOR_4Byte), $ne8),
                            nqp::writeuint($buf, $pos, $value, $be32),
                            ($pos = nqp::add_I(nqp::decont($pos), 4, Int)),
                        ),
                        nqp::stmts(
                            nqp::writeuint($buf, $pos++, nqp::add_i($major-type, CBOR_8Byte), $ne8),
                            nqp::writeuint($buf, $pos, $value, $be64),
                            ($pos = nqp::add_I(nqp::decont($pos), 8, Int)),
                        )
                    )
                )
            )
        )
    }

    my sub write-medium-uint(int $major-type, $value) {
        nqp::writeuint($buf, $pos++, $major-type + CBOR_8Byte, $ne8);
        $buf.write-uint64($pos, $value, BigEndian);
        # $pos += 8;
        $pos = nqp::add_I(nqp::decont($pos), 8, Int);
    }

    my &encode = -> $value {
    # Defined values
        with $value {
            # First classify by general role, then by actual type

            # Check for Numeric before Stringy so allomorphs prefer Numeric
            if nqp::istype($_, Numeric) {
                if nqp::istype($_, Bool) {
                    nqp::writeuint($buf, $pos++, ($_ ?? CBOR_SVal + CBOR_True
                                                     !! CBOR_SVal + CBOR_False), $ne8);
                }
                elsif nqp::istype($_, Int) {
                    # Small int
                    if CBOR_Min_NInt_63Bit <= $_ <= CBOR_Max_UInt_63Bit {
                        $_ >= 0 ?? write-uint(CBOR_UInt,   $_)
                                !! write-uint(CBOR_NInt, +^$_);
                    }
                    # Medium int
                    elsif CBOR_Min_NInt_8Byte <= $_ <= CBOR_Max_UInt_8Byte {
                        $_ >= 0 ?? write-medium-uint(CBOR_UInt,   $_)
                                !! write-medium-uint(CBOR_NInt, +^$_);
                    }
                    # Unsigned BigInt
                    elsif $_ >= 0 {
                        nqp::writeuint($buf, $pos++, CBOR_Tag + CBOR_Tag_Unsigned_BigInt, $ne8);
                        my @bytes = .polymod(256 xx *).reverse;
                        my $bytes = @bytes.elems;
                        write-uint(CBOR_BStr, $bytes);
                        $buf.splice($pos, $bytes, @bytes);
                        $pos += $bytes;
                    }
                    # Negative BigInt
                    else {
                        nqp::writeuint($buf, $pos++, CBOR_Tag + CBOR_Tag_Negative_BigInt, $ne8);
                        my @bytes = (+^$_).polymod(256 xx *).reverse;
                        my $bytes = @bytes.elems;
                        write-uint(CBOR_BStr, $bytes);
                        $buf.splice($pos, $bytes, @bytes);
                        $pos += $bytes;
                    }
                }
                elsif nqp::istype($_, Num) {
                    # Handle NaN and ±Inf separately, so others can be fast-pathed
                    if nqp::isnanorinf($_) {
                        # ±Inf case (NaN is never equal to itself)
                        if nqp::iseq_n($_, $_) {
                            # Two-byte encoding, only sign bit differs
                            nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_2Byte, $ne8);
                            nqp::writeuint($buf, $pos++, $_ > 0 ?? 0x7C !! 0xFC, $ne8);
                            nqp::writeuint($buf, $pos++, 0, $ne8);
                        }
                        # NaN case, complicated by requirement to retain significand info
                        else {
                            my buf8 $nan .= new;
                            $nan.write-num64(0, $_, BigEndian);

                            if $nan[4] == $nan[5] == $nan[6] == $nan[7] == 0 {
                                # 4-byte NaN required
                                if $nan[3] || $nan[2] +& 3 {
                                    my num32 $nan32 = $_;
                                    nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_4Byte, $ne8);
                                    nqp::writenum($buf, $pos, $nan32, $be32);

                                    # Canonify NaN sign bit to 0, even on platforms with -NaN
                                    nqp::writeuint($buf, $pos, nqp::readuint($buf, $pos, $ne8) +& 0x7F, $ne8);
                                    # $pos += 4;
                                    $pos = nqp::add_I(nqp::decont($pos), 4, Int);
                                }
                                # 2-byte NaN sufficient
                                else {
                                    nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_2Byte, $ne8);
                                    nqp::writeuint($buf, $pos, bin16-from-num($_), $be16);
                                    # $pos += 4;
                                    $pos = nqp::add_I(nqp::decont($pos), 4, Int);
                                }
                            }
                            # 8-byte NaN required
                            else {
                                nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_8Byte, $ne8);
                                nqp::writenum($buf, $pos, $_, $be64);

                                # Canonify NaN sign bit to 0, even on platforms with -NaN
                                nqp::writeuint($buf, $pos, nqp::readuint($buf, $pos, $ne8) +& 0x7F, $ne8);
                                # $pos += 8;
                                $pos = nqp::add_I(nqp::decont($pos), 8, Int);
                            }
                        }
                    }
                    # Can safely shrink to 32 bits with no loss of information
                    elsif nqp::iseq_n($_, (my num32 $num32 = $_)) {
                        # XXXX: 16-bit num support path
                        # my $bin16 = bin16-from-num($_);
                        # my $num16 = num-from-bin16($bin16);
                        # if $num16 == $_ {
                        #     nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_2Byte, $ne8);
                        #     nqp::writeuint($buf, $pos, $bin16, $be16);
                        #     $pos += 2;
                        # }
                        nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_4Byte, $ne8);
                        nqp::writenum($buf, $pos, $num32, $be32);
                        # $pos += 4;
                        $pos = nqp::add_I(nqp::decont($pos), 4, Int);
                    }
                    # Needs full 64-bit num for round trip
                    else {
                        nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_8Byte, $ne8);
                        nqp::writenum($buf, $pos, $_, $be64);
                        # $pos += 8;
                        $pos = nqp::add_I(nqp::decont($pos), 8, Int);
                    }
                }
                elsif nqp::istype($_, Rational) {
                    # write-uint(CBOR_Tag, CBOR_Tag_Rational);
                    nqp::writeuint($buf, $pos++, CBOR_Tag + CBOR_1Byte, $ne8);
                    nqp::writeuint($buf, $pos++, CBOR_Tag_Rational, $ne8);
                    nqp::writeuint($buf, $pos++, CBOR_Array + 2, $ne8);

                    my $nu := .numerator;
                    # Slow path for FatRats and "big" Rats
                    if nqp::istype($_, FatRat)
                    || $nu > CBOR_Max_UInt_63Bit
                    || $nu < CBOR_Min_NInt_63Bit {
                        encode($nu);
                        encode(.denominator);
                    }
                    # Fast path for "small" Rats
                    else {
                        $nu >= 0 ?? write-uint(CBOR_UInt,   $nu)
                                 !! write-uint(CBOR_NInt, +^$nu);
                        write-uint(CBOR_UInt, .denominator);
                    }
                }
                elsif nqp::istype($_, Instant) {
                    my $num = .to-posix[0].Num;
                    my $val = $num.Int == $num ?? $num.Int !! $num;

                    nqp::writeuint($buf, $pos++, CBOR_Tag + CBOR_Tag_DateTime_Number, $ne8);
                    encode($val);
                }
                elsif nqp::istype($_, Real) {
                    # XXXX: Pretend any other Real is a Num
                    encode(.Num);
                }
                else {
                    my $ex = "Don't know how to encode a {$value.^name}";
                    $*CBOR_SIMPLE_FATAL_ERRORS ?? die $ex !! fail $ex;
                }
            }
            elsif nqp::istype($_, Stringy) {
                if nqp::istype($_, Str) {
                    write-uint(CBOR_TStr,
                               my $bytes := nqp::elems(
                                   my $utf8 := $utf8-encoder.encode-chars($_)));

                    nqp::splice($buf, $utf8, $pos, $bytes);
                    $pos = nqp::add_I(nqp::decont($pos), $bytes, Int);
                }
                elsif nqp::istype($_, Blob) {
                    write-uint(CBOR_BStr, my $bytes := .bytes);
                    $buf.splice($pos, $bytes, $_);
                    $pos = nqp::add_I(nqp::decont($pos), $bytes, Int);
                }
                else {
                    my $ex = "Don't know how to encode a {$value.^name}";
                    $*CBOR_SIMPLE_FATAL_ERRORS ?? die $ex !! fail $ex;
                }
            }
            elsif nqp::istype($_, Capture) {
                write-uint(CBOR_Tag, CBOR_Tag_Capture);
                my $list := .list;
                my $hash := .hash;

                # Use preferred serialization, skipping empty children
                encode($list ?? ($hash ?? [$list, $hash] !! [$list,])
                             !! ($hash ?? [$hash,]       !! []      ));
            }
            # XXXX: Seq/Iterator?
            elsif nqp::istype($_, Positional) {
                constant $endian    = Kernel.endian == LittleEndian ?? 4 !! 0;
                constant $nan-fix   = (buf8.new.write-num64(0, NaN, BigEndian)
                                       .read-uint8(0)) +& 0x80;
                constant %type-info =
                    #          TAG         SHIFT  BYTES  CONSTANT
                    byte   => (0x40,           0, 1, nqp::const::BINARY_SIZE_8_BIT),
                    uint8  => (0x40,           0, 1, nqp::const::BINARY_SIZE_8_BIT),
                    uint16 => (0x41 + $endian, 1, 2, nqp::const::BINARY_SIZE_16_BIT),
                    uint32 => (0x42 + $endian, 2, 4, nqp::const::BINARY_SIZE_32_BIT),
                    uint64 => (0x43 + $endian, 3, 8, nqp::const::BINARY_SIZE_64_BIT),
                    uint   => (0x43 + $endian, 3, 8, nqp::const::BINARY_SIZE_64_BIT),

                    int8   => (0x48,           0, 1, nqp::const::BINARY_SIZE_8_BIT),
                    int16  => (0x49 + $endian, 1, 2, nqp::const::BINARY_SIZE_16_BIT),
                    int32  => (0x4A + $endian, 2, 4, nqp::const::BINARY_SIZE_32_BIT),
                    int64  => (0x4B + $endian, 3, 8, nqp::const::BINARY_SIZE_64_BIT),
                    int    => (0x4B + $endian, 3, 8, nqp::const::BINARY_SIZE_64_BIT),

                    num32  => (0x51 + $endian, 2, 4, nqp::const::BINARY_SIZE_32_BIT),
                    num64  => (0x52 + $endian, 3, 8, nqp::const::BINARY_SIZE_64_BIT),
                    num    => (0x52 + $endian, 3, 8, nqp::const::BINARY_SIZE_64_BIT);

                # Try to pack native arrays using RFC 8746 Typed Arrays tag
                if nqp::istype($_, array) {
                    my $array     := $_<>;
                    my $type       = $array.of;
                    my $type-info  = %type-info{$type.^name};
                    my int $elems  = $array.elems;

                    # Packed array types supported in RFC 8746
                    if $type-info {
                        my $tag   = $type-info[0];
                        write-uint(CBOR_Tag, $tag);

                        my $bytes = $elems * $type-info[2];
                        write-uint(CBOR_BStr, $bytes);

                        my int $s = $type-info[1];
                        my int $t = nqp::bitor_i($type-info[3], NativeEndian);
                        my int $p = $pos;
                        my int $i = -1;

                        # num* types
                        if $tag >= 0x50 {
                            # Slow(er) path for systems with signed NaN
                            if $nan-fix {
                                my int $sign-offset = $endian ?? $type-info[2] - 1 !! 0;
                                nqp::while(
                                    nqp::islt_i(($i = nqp::add_i($i, 1)), $elems),
                                    nqp::stmts(
                                        (my int $l = nqp::add_i($p, nqp::bitshiftl_i($i, $s))),
                                        (my num $n = nqp::atpos_n($array, $i)),
                                        (nqp::writenum($buf, $l, $n, $t)),
                                        # Only do sign fix for NaN (!= self)
                                        (nqp::if(nqp::isne_n($n, $n),
                                                 nqp::stmts(
                                                     (my int $sl = nqp::add_i($l, $sign-offset)),
                                                     (nqp::writeuint($buf, $sl,
                                                                     nqp::readuint($buf, $sl, $ne8) +& 0x7F, $ne8)),
                                                 ))
                                        ),
                                    )
                                );
                            }
                            # Fast path for systems that don't need NaN fix
                            else {
                                nqp::while(
                                    nqp::islt_i(($i = nqp::add_i($i, 1)), $elems),
                                    nqp::writenum($buf,
                                                  nqp::add_i($p, nqp::bitshiftl_i($i, $s)),
                                                  nqp::atpos_n($array, $i),
                                                  $t)
                                );
                            }
                        }
                        # (signed) int* types
                        elsif $tag >= 0x48 {
                            if $s {
                                nqp::while(
                                    nqp::islt_i(($i = nqp::add_i($i, 1)), $elems),
                                    nqp::writeint($buf,
                                                  nqp::add_i($p, nqp::bitshiftl_i($i, $s)),
                                                  nqp::atpos_i($array, $i),
                                                  $t)
                                );
                            }
                            else {
                                nqp::while(
                                    nqp::islt_i(($i = nqp::add_i($i, 1)), $elems),
                                    nqp::writeint($buf,
                                                  nqp::add_i($p, $i),
                                                  nqp::atpos_i($array, $i),
                                                  $t)
                                );
                            }
                        }
                        # uint* types
                        else {
                            if $s {
                                nqp::while(
                                    nqp::islt_i(($i = nqp::add_i($i, 1)), $elems),
                                    nqp::writeuint($buf,
                                                   nqp::add_i($p, nqp::bitshiftl_i($i, $s)),
                                                   nqp::atpos_i($array, $i),
                                                   $t)
                                );
                            }
                            else {
                                nqp::while(
                                    nqp::islt_i(($i = nqp::add_i($i, 1)), $elems),
                                    nqp::writeuint($buf,
                                                   nqp::add_i($p, $i),
                                                   nqp::atpos_i($array, $i),
                                                   $t)
                                );
                            }
                        }

                        $pos += $bytes;
                    }
                    else {
                        # Fake other packed array types (e.g. strarray)
                        # by writing them as standard CBOR Arrays instead
                        write-uint(CBOR_Array, $elems);
                        encode($_) for @$_;
                    }
                }
                # Treat all other Positional types as standard Arrays
                else {
                    write-uint(CBOR_Array, .elems);
                    encode($_) for @$_;
                }
            }
            elsif nqp::istype($_, Associative) {
                if nqp::istype($_, Setty) {
                    write-uint(CBOR_Tag, CBOR_Tag_Set);
                    encode(.keys.sort.cache);
                }
                else {
                    write-uint(CBOR_Map, .elems);
                    if RFC8949_Map_Key_Sort {
                        my @pairs = .map: {
                            cbor-encode(.key, my $ = 0) => .value
                        };
                        @pairs.sort(*.key).map: {
                            my $bytes = .key.bytes;
                            $buf.splice($pos, $bytes, .key);
                            $pos += $bytes;
                            encode(.value);
                        }
                    }
                    else {
                        for .sort {
                            encode(.key);
                            encode(.value);
                        }
                    }
                }
            }
            elsif nqp::istype($_, Dateish) {
                if nqp::istype($_, DateTime) {
                    my $num = .Instant.to-posix[0].Num;
                    my $val = $num.Int == $num ?? $num.Int !! $num;

                    nqp::writeuint($buf, $pos++, CBOR_Tag + CBOR_Tag_DateTime_Number, $ne8);
                    encode($val);
                }
                else {
                    nqp::writeuint($buf, $pos++, CBOR_Tag + CBOR_1Byte, $ne8);
                    nqp::writeuint($buf, $pos++, CBOR_Tag_Date_Integer, $ne8);
                    encode(.daycount - 40587);  # Raku MJD -> RFC 8943 days
                }
            }
            elsif nqp::istype($_, Tagged) {
                write-uint(CBOR_Tag, .tag-number);
                encode(.value);
            }
            else {
                my $ex = "Don't know how to encode a {$value.^name}";
                $*CBOR_SIMPLE_FATAL_ERRORS ?? die $ex !! fail $ex;
            }
        }
        # Undefined values
        else {
            if nqp::istype($_, Nil) {
                nqp::writeuint($buf, $pos++, CBOR_Tag  + CBOR_1Byte, $ne8);
                nqp::writeuint($buf, $pos++, CBOR_Tag_Absent,        $ne8);
                nqp::writeuint($buf, $pos++, CBOR_SVal + CBOR_Undef, $ne8);
            }
            else {
                # Any:U is CBOR null, other Mu:U is CBOR undefined
                nqp::writeuint($buf, $pos++, CBOR_SVal + (nqp::istype($value, Any)
                                                          ?? CBOR_Null !! CBOR_Undef), $ne8);
            }
        }
    }

    encode($value);
    $buf
}


# Decode the first value from CBOR-encoded data
multi cbor-decode(Blob:D $cbor) is export {
    my $value := cbor-decode($cbor, my $pos = 0);
    if $pos < $cbor.bytes {
        my $ex = X::Malformed.new(:payload("Extra data after decoded value"));
        $*CBOR_SIMPLE_FATAL_ERRORS ?? die $ex !! fail $ex;
    }
    $value
}

# Decode the next value from CBOR-encoded data, starting at $pos
multi cbor-decode(Blob:D $cbor, Int:D $pos is rw, Bool:D :$breakable = False) is export {
    my &fail-malformed = -> Str:D $reason {
        my $ex = X::Malformed.new(:payload($reason));
        $*CBOR_SIMPLE_FATAL_ERRORS ?? die $ex !! fail $ex;
    }

    CATCH {
        when /^ 'MVMArray: read_buf out of bounds' / {
            fail-malformed "Early end of input";
        }
        default { .rethrow }
    }

    my int $cbor-length = $cbor.bytes;
    my int $argument;

    # This gets called by almost all definite value decoders, so go for speed
    my &read-uint = {
        my int $v;
        nqp::if(
            nqp::islt_i($argument, CBOR_1Byte),
            $argument,
            nqp::if(
                nqp::iseq_i($argument, CBOR_1Byte),
                nqp::readuint($cbor, $pos++, $ne8),
                nqp::if(
                    nqp::iseq_i($argument, CBOR_2Byte),
                    nqp::stmts(
                        ($v = nqp::readuint($cbor, $pos, $be16)),
                        ($pos = nqp::add_I(nqp::decont($pos), 2, Int)),
                        $v
                    ),
                    nqp::if(
                        nqp::iseq_i($argument, CBOR_4Byte),
                        nqp::stmts(
                            ($v = nqp::readuint($cbor, $pos, $be32)),
                            ($pos = nqp::add_I(nqp::decont($pos), 4, Int)),
                            $v
                        ),
                        nqp::if(
                            nqp::iseq_i($argument, CBOR_8Byte),
                            nqp::stmts(
                                (my $v64 = nqp::readuint($cbor, $pos, $be64)),
                                ($pos = nqp::add_I(nqp::decont($pos), 8, Int)),
                                $v64
                            ),
                            fail-malformed("Invalid argument $argument")
                        )
                    )
                )
            )
        )
    }

    my &decode-bstr = {
        # Indefinite length
        if $argument == CBOR_Indefinite_Break && !$breakable {
            my buf8 $joined .= new;
            until (my $chunk := cbor-decode($cbor, $pos, :breakable)) =:= Break {
                fail-malformed "Byte string chunk has wrong type"
                    unless nqp::istype($chunk, Buf);
                $joined.append($chunk);
            }
            $joined
        }
        # Definite length
        elsif (my $bytes = read-uint) {
            fail-malformed "Unreasonably long byte string"
                if $bytes > CBOR_Max_UInt_63Bit;

            fail-malformed "Byte string too short"
                unless $cbor-length >= (my $a = $pos + $bytes);

            my $buf := nqp::slice($cbor, $pos, $a - 1);
            $pos = $a;
            $buf
        }
        else { buf8.new }
    }

    my &decode-tstr = {
        # Indefinite length
        if nqp::iseq_i($argument, CBOR_Indefinite_Break) && !$breakable {
            my @chunks;
            until (my $chunk := cbor-decode($cbor, $pos, :breakable)) =:= Break {
                fail-malformed "Text string chunk has wrong type"
                    unless nqp::istype($chunk, Str);
                @chunks.push($chunk);
            }
            @chunks.join
        }
        # Definite length
        elsif (my $bytes := read-uint) {
            fail-malformed "Unreasonably long text string"
                if $bytes > CBOR_Max_UInt_63Bit;

            fail-malformed "Text string too short"
                unless $cbor-length >= (my $a := $pos + $bytes);

            my $str := nqp::p6box_s(nqp::decode(nqp::slice($cbor, $pos, $a - 1), 'utf8'));
            $pos = $a;
            $str
        }
        else { '' }
    }

    my &decode-array = {
        # Indefinite length
        $argument == CBOR_Indefinite_Break
        ?? do {
            my @array;
            until (my $item := cbor-decode($cbor, $pos, :breakable)) =:= Break {
                @array.push($item);
            }
            @array
        }
        !! my @ = (^read-uint).map(&decode)
    }

    my &decode-map = {
        my %str-map;
        my %mu-map{Mu};

        # Indefinite length
        if nqp::iseq_i($argument, CBOR_Indefinite_Break) {
            until (my $k := cbor-decode($cbor, $pos, :breakable)) =:= Break {
                nqp::istype($k, Str) ?? %str-map.ASSIGN-KEY($k, decode)
                                     !! (%mu-map.AT-KEY($k) = decode);
            }
        }
        # Definite length
        else {
            my int $elems = read-uint;
            my int $i     = 0;

            nqp::while(
                nqp::isle_i($i = nqp::add_i($i, 1), $elems),
                nqp::if(
                    nqp::istype((my $k = decode), Str),
                    %str-map.ASSIGN-KEY($k, decode),
                    (%mu-map.AT-KEY($k) = decode)
                )
            );
        }

        %mu-map.elems
        ?? nqp::stmts((%mu-map{$_} = %str-map{$_} for %str-map.keys), %mu-map)
        !! %str-map
    }

    my &decode-sval = {
        my constant %svals = 20 => False, 21 => True, 22 => Any, 23 => Mu;

        if $argument <= CBOR_Undef {
            $argument < CBOR_False
                ?? fail-malformed("Unassigned simple value $argument")
                !! %svals{$argument}
        }
        elsif $argument == CBOR_8Byte {
            my num64 $v = $cbor.read-num64($pos, BigEndian);
            # $pos += 8;
            $pos = nqp::add_I(nqp::decont($pos), 8, Int);
            $v
        }
        elsif $argument == CBOR_4Byte {
            my num32 $v = $cbor.read-num32($pos, BigEndian);
            # $pos += 4;
            $pos = nqp::add_I(nqp::decont($pos), 8, Int);
            $v
        }
        elsif $argument == CBOR_2Byte {
            my num32 $v = num-from-bin16($cbor.read-uint16($pos, BigEndian));
            # $pos += 2;
            $pos = nqp::add_I(nqp::decont($pos), 8, Int);
            $v
        }
        elsif $argument == CBOR_1Byte {
            my $val  = nqp::readuint($cbor, $pos++, $ne8);
            my $fail = $val < 24 ?? "Badly formed" !!
                       $val < 32 ?? "Reserved"     !!
                                    "Unassigned"   ;
            fail-malformed "$fail simple value $val";
        }
        elsif $argument == CBOR_Indefinite_Break {
            $breakable ?? Break
                       !! fail-malformed "Unexpected break signal";
        }
        else {
            fail-malformed "Badly formed simple value $argument";
        }
    }

    my &decode-tag = {
        my $tag-number = read-uint;
        if 64 <= $tag-number <= 87 {  # RFC 8746 Typed Arrays; details bit-coded in tag
            # Decode tag
            my $is-float         = $tag-number +& 24 == 16;
            my $is-signed        = $tag-number +& 8;
            my $is-little-endian = $tag-number +& 4;
            my $size             = 1 +< ($tag-number +& 3 + $is-float);

            # Determine Endian type to read with
            my $on-little-endian = Kernel.endian == LittleEndian;
            my $is-same-endian   = !($is-little-endian ?^ $on-little-endian);
            my $endian           = $is-same-endian   ?? NativeEndian !!
                                   $on-little-endian ?? BigEndian    !!
                                                        LittleEndian;

            # Look at tagged content and check that it is a byte string
            $argument = nqp::bitand_i(
                my int $initial-byte = nqp::readuint($cbor, $pos++, $ne8),
                CBOR_Argument_Mask);
            fail-malformed "Typed Array tag ($tag-number) does not contain a byte string"
                unless nqp::bitand_i($initial-byte, CBOR_MajorType_Mask) == CBOR_BStr;

            # Check that the byte string has an even number of elements
            my $bytes = read-uint;
            fail-malformed "Typed Array with element size $size does not evenly divide byte length $bytes"
                if $bytes % $size;

            # Determine actual element count
            my int $elems = $bytes div $size;

            # Parse out the actual array
            if $is-float {
                if $size == 2 {
                    my $array := array[num32].new;

                    # Presize array to reduce copying
                    nqp::setelems($array, $elems);

                    # We can't just memcopy, so apply NQP afterburners instead
                    my int $p = $pos;
                    my int $t = nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT, $endian);
                    my int $i = -1;
                    nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_n($array, $i,
                                       num-from-bin16(
                                           nqp::readuint($cbor,
                                                         nqp::add_i($p, nqp::bitshiftl_i($i,1)),
                                                         $t)))
                    );

                    $pos += $bytes;
                    $array
                }
                elsif $size == 4 {
                    my $array := array[num32].new;

                    # Presize array to reduce copying
                    nqp::setelems($array, $elems);

                    # We can't just memcopy, so apply NQP afterburners instead
                    my int $p = $pos;
                    my int $t = nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT, $endian);
                    my int $i = -1;
                    nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_n($array, $i,
                                       nqp::readnum($cbor,
                                                    nqp::add_i($p,
                                                               nqp::bitshiftl_i($i, 2)),
                                                    $t))
                    );

                    $pos += $bytes;
                    $array
                }
                elsif $size == 8 {
                    my $array := array[num64].new;

                    # Presize array to reduce copying
                    nqp::setelems($array, $elems);

                    # We can't just memcopy, so apply NQP afterburners instead
                    my int $p = $pos;
                    my int $t = nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT, $endian);
                    my int $i = -1;
                    nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_n($array, $i,
                                       nqp::readnum($cbor,
                                                    nqp::add_i($p,
                                                               nqp::bitshiftl_i($i, 3)),
                                                    $t))
                    );

                    $pos += $bytes;
                    $array
                }
                else {
                    die "Unable to parse native float array with element size $size";
                }
            }
            else {
                if $size == 1 {
                    fail-malformed "CBOR extension tag 76 indicating signed 8-bit little-endian packed array is reserved by RFC 8746" if $tag-number == 76;

                    my $array := array[$is-signed ?? int8 !! uint8].new;

                    # Presize array to reduce copying
                    nqp::setelems($array, $elems);

                    # We can't just memcopy, so apply NQP afterburners instead
                    my int $p = $pos;
                    my int $i = -1;
                    $is-signed
                    ?? nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readint($cbor,
                                                    nqp::add_i($p, $i),
                                                    $ne8))
                       )
                    !! nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readuint($cbor,
                                                     nqp::add_i($p, $i),
                                                     $ne8))
                       );

                    $pos += $bytes;
                    $array
                }
                elsif $size == 2 {
                    my $array := array[$is-signed ?? int16 !! uint16].new;

                    # Presize array to reduce copying
                    nqp::setelems($array, $elems);

                    # We can't just memcopy, so apply NQP afterburners instead
                    my int $p = $pos;
                    my int $t = nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT, $endian);
                    my int $i = -1;
                    $is-signed
                    ?? nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readint($cbor,
                                                    nqp::add_i($p,
                                                               nqp::bitshiftl_i($i, 1)),
                                                    $t))
                       )
                    !! nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readuint($cbor,
                                                     nqp::add_i($p,
                                                                nqp::bitshiftl_i($i, 1)),
                                                     $t))
                       );

                    $pos += $bytes;
                    $array
                }
                elsif $size == 4 {
                    my $array := array[$is-signed ?? int32 !! uint32].new;

                    # Presize array to reduce copying
                    nqp::setelems($array, $elems);

                    # We can't just memcopy, so apply NQP afterburners instead
                    my int $p = $pos;
                    my int $t = nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT, $endian);
                    my int $i = -1;
                    $is-signed
                    ?? nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readint($cbor,
                                                    nqp::add_i($p,
                                                               nqp::bitshiftl_i($i, 2)),
                                                    $t))
                       )
                    !! nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readuint($cbor,
                                                     nqp::add_i($p,
                                                                nqp::bitshiftl_i($i, 2)),
                                                     $t))
                       );

                    $pos += $bytes;
                    $array
                }
                elsif $size == 8 {
                    my $array := array[$is-signed ?? int64 !! uint64].new;

                    # Presize array to reduce copying
                    nqp::setelems($array, $elems);

                    # We can't just memcopy, so apply NQP afterburners instead
                    my int $p = $pos;
                    my int $t = nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT, $endian);
                    my int $i = -1;
                    $is-signed
                    ?? nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readint($cbor,
                                                    nqp::add_i($p,
                                                               nqp::bitshiftl_i($i, 3)),
                                                    $t))
                       )
                    !! nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        nqp::bindpos_i($array, $i,
                                       nqp::readuint($cbor,
                                                     nqp::add_i($p,
                                                                nqp::bitshiftl_i($i, 3)),
                                                     $t))
                       );

                    $pos += $bytes;
                    $array
                }
            }
        }
        elsif $tag-number == CBOR_Tag_Rational {
            fail-malformed "Rational tag (30) does not contain an array with exactly two elements"
                unless nqp::readuint($cbor, $pos++, $ne8) == CBOR_Array + 2;

            my $nu = decode;
            my $de = decode;
            fail-malformed "Rational tag (30) numerator is not an integer"
                unless nqp::istype($nu, Int);
            fail-malformed "Rational tag (30) denominator is not a positive integer"
                unless nqp::istype($de, Int) && $de > 0;

            $de <= CBOR_Max_UInt_8Byte ?? Rat.new(   $nu, $de)
                                       !! FatRat.new($nu, $de)
        }
        elsif $tag-number == CBOR_Tag_DateTime_Number {
            my $seconds := decode;
            fail-malformed "Epoch DateTime tag(1) does not contain a real number"
                unless nqp::istype($seconds, Real);
            Instant.from-posix($seconds) // fail-malformed "Epoch DateTime could not be decoded"
        }
        elsif $tag-number == CBOR_Tag_DateTime_String {
            my $dt := decode;
            fail-malformed "DateTime tag (0) does not contain a string"
                unless nqp::istype($dt, Str);
            DateTime.new($dt) // fail-malformed "DateTime string could not be parsed"
        }
        elsif $tag-number == CBOR_Tag_Date_Integer {
            my $days := decode;
            fail-malformed "Gregorian days tag(100) does not contain an integer"
                unless nqp::istype($days, Int);
            Date.new-from-daycount($days + 40587) // fail-malformed "Gregorian days could not be decoded"
        }
        elsif $tag-number == CBOR_Tag_Date_String {
            my $date := decode;
            fail-malformed "Date string tag (1004) does not contain a string"
                unless nqp::istype($date, Str);
            Date.new($date) // fail-malformed "Date string could not be parsed"
        }
        elsif $tag-number == CBOR_Tag_Unsigned_BigInt {
            my $bytes := decode;
            fail-malformed "Unsigned BigInt does not contain a byte string"
                unless nqp::istype($bytes, Buf);
            my $value = 0;
            $value = $value * 256 + $_ for @$bytes;
            $value
        }
        elsif $tag-number == CBOR_Tag_Negative_BigInt {
            my $bytes := decode;
            fail-malformed "Negative BigInt does not contain a byte string"
                unless nqp::istype($bytes, Buf);
            my $value = 0;
            $value = $value * 256 + $_ for @$bytes;
            +^$value
        }
        elsif $tag-number == CBOR_Tag_Capture {
            my $capture_top = nqp::readuint($cbor, $pos++, $ne8);

            if    $capture_top == CBOR_Array {
                Capture.new()
            }
            elsif $capture_top == CBOR_Array + 1 {
                my $child := decode;
                $child := $child.value
                       if nqp::istype($child, Tagged)
                       && (   $child.tag-number == CBOR_Tag_Object_Key_Map
                           || $child.tag-number == CBOR_Tag_String_Key_Map);
                if nqp::istype($child, Array) {
                    Capture.new(:list($child))
                }
                elsif nqp::istype($child, Map) {
                    Capture.new(:hash($child))
                }
                else {
                    fail-malformed "Capture tag ($tag-number) array's child is neither an array nor a map";
                }
            }
            elsif $capture_top == CBOR_Array + 2 {
                my $list := decode;
                my $hash := decode;
                   $hash := $hash.value
                         if nqp::istype($hash, Tagged)
                         && (   $hash.tag-number == CBOR_Tag_Object_Key_Map
                             || $hash.tag-number == CBOR_Tag_String_Key_Map);

                fail-malformed "Capture tag ($tag-number) array's first element is not an array"
                    unless nqp::istype($list, Array);
                fail-malformed "Capture tag ($tag-number) array's second element is not a map"
                    unless nqp::istype($hash, Map);

                Capture.new(:$list, :$hash)
            }
            else {
                fail-malformed "Capture tag ($tag-number) does not contain an array with at most two elements";
            }
        }
        elsif $tag-number == CBOR_Tag_Set {
            fail-malformed "Set tag (258) does not contain an array"
                unless nqp::readuint($cbor, $pos, $ne8) +& CBOR_MajorType_Mask == CBOR_Array;
            (decode).Set
        }
        elsif $tag-number == CBOR_Tag_Object_Key_Map {
            fail-malformed "Map with object keys tag (259) does not contain a map"
                unless nqp::readuint($cbor, $pos, $ne8) +& CBOR_MajorType_Mask == CBOR_Map;
            my %map := decode;
            if nqp::istype(%map.keyof, Str) {
                my %mu-map{Mu};
                %mu-map{$_} = %map{$_} for %map.keys;
                %mu-map
            }
            else {
                %map
            }
        }
        elsif $tag-number == CBOR_Tag_String_Key_Map {
            fail-malformed "Map with string keys tag (275) does not contain a map"
                unless nqp::readuint($cbor, $pos, $ne8) +& CBOR_MajorType_Mask == CBOR_Map;
            my %map := decode;
            fail-malformed "Map with string keys tag (275) contains non-string keys"
                unless nqp::istype(%map.keyof, Str);
            %map
        }
        elsif $tag-number == CBOR_Tag_Decimal_Fraction
           || $tag-number == CBOR_Tag_Decimal_Extended {
            fail-malformed "Decimal Fraction tag ($tag-number) does not contain an array with exactly two elements"
                unless nqp::readuint($cbor, $pos++, $ne8) == CBOR_Array + 2;

            my $exp = decode;
            my $man = decode;
            fail-malformed "Decimal Fraction tag (4) exponent is not an integer"
                unless nqp::istype($exp, Int);
            fail-malformed "Decimal Fraction tag (4) mantissa is not an integer"
                unless nqp::istype($man, Int);

            $exp >= 0 ?? $man * 10 ** $exp !! do {
                my $de = 10 ** -$exp;
                $de <= CBOR_Max_UInt_8Byte ?? Rat.new(   $man, $de)
                                           !! FatRat.new($man, $de)
            }
        }
        elsif $tag-number == CBOR_Tag_Bigfloat
           || $tag-number == CBOR_Tag_Bigfloat_Extended {
            fail-malformed "Bigfloat tag ($tag-number) does not contain an array with exactly two elements"
                unless nqp::readuint($cbor, $pos++, $ne8) == CBOR_Array + 2;

            my $exp = decode;
            my $man = decode;
            fail-malformed "Bigfloat tag (5) exponent is not an integer"
                unless nqp::istype($exp, Int);
            fail-malformed "Bigfloat tag (5) mantissa is not an integer"
                unless nqp::istype($man, Int);

            $exp >= 0 ?? $man * 2 ** $exp !! do {
                my $de = 2 ** -$exp;
                $de <= CBOR_Max_UInt_8Byte ?? Rat.new(   $man, $de)
                                           !! FatRat.new($man, $de)
            }
        }
        elsif $tag-number == CBOR_Tag_Absent {
            fail-malformed "Absent tag (31) does not contain the undefined value"
                unless nqp::readuint($cbor, $pos++, $ne8) == CBOR_SVal + CBOR_Undef;
            Nil
        }
        # Lazy decoding: byte string containing CBOR-encoded data that is NOT unwrapped
        elsif $tag-number == CBOR_Tag_Encoded_CBOR
           || $tag-number == CBOR_Tag_Encoded_Sequence {
            fail-malformed "Encoded CBOR tag ($tag-number) does not contain a byte string"
                unless nqp::readuint($cbor, $pos, $ne8) +& CBOR_MajorType_Mask == CBOR_BStr;
            Tagged.new(:$tag-number, :value(decode))
        }
        # Self-tagged CBOR item or CBOR Sequence, just unwrap the decoded tag content
        elsif $tag-number == CBOR_Tag_Self_Described
           || $tag-number == CBOR_Tag_Self_Sequence {
            decode
        }
        # Intentionally (as per spec) invalid tag values
        elsif $tag-number == CBOR_Tag_Invalid_2Byte
           || $tag-number == CBOR_Tag_Invalid_4Byte
           || $tag-number == CBOR_Tag_Invalid_8Byte {
            fail-malformed "Multi-byte tag number has all bits on";
        }
        # Final fallback: Just wrapped the value in a CBOR::Simple::Tagged object
        else {
            Tagged.new(:$tag-number, :value(decode))
        }
    }

    my @decoders =
        &read-uint,
        { +^read-uint },
        &decode-bstr,
        &decode-tstr,
        &decode-array,
        &decode-map,
        &decode-tag,
        &decode-sval;

    my &decode = {
        $argument = nqp::bitand_i(
            my int $initial-byte = nqp::readuint($cbor, $pos++, $ne8),
            CBOR_Argument_Mask);

        @decoders.AT-POS(nqp::bitshiftr_i(nqp::bitand_i($initial-byte,
                                                        CBOR_MajorType_Mask), 5)).()
    }

    decode;
}


# Convert a CBOR-encoded value to human diagnostic form
multi cbor-diagnostic(Blob:D $cbor) is export {
    cbor-diagnostic($cbor, my $pos = 0)
}

# Convert a CBOR-encoded value to human diagnostic form, starting at $pos
multi cbor-diagnostic(Blob:D $cbor, Int:D $pos is rw, Bool:D :$breakable = False) is export {
    my $initial-byte = $cbor.read-uint8($pos++);
    my $major-type   = $initial-byte +& CBOR_MajorType_Mask;
    my $argument     = $initial-byte +& CBOR_Argument_Mask;

    my &read-uint = {
        if $argument < CBOR_1Byte {
            $argument
        }
        elsif $argument == CBOR_1Byte {
            $cbor.read-uint8($pos++)
        }
        elsif $argument == CBOR_2Byte {
            my $v = $cbor.read-uint16($pos, BigEndian);
            $pos += 2;
            $v
        }
        elsif $argument == CBOR_4Byte {
            my $v = $cbor.read-uint32($pos, BigEndian);
            $pos += 4;
            $v
        }
        elsif $argument == CBOR_8Byte {
            my $v = $cbor.read-uint64($pos, BigEndian);
            $pos += 8;
            $v
        }
        elsif $argument == CBOR_Indefinite_Break {
            Whatever
        }
        else {
            fail "argument($argument)"
        }
    }

    given $major-type {
        when CBOR_UInt {
            ~read-uint
        }
        when CBOR_NInt {
            ~(+^read-uint)
        }
        when CBOR_BStr {
            my $bytes = read-uint;

            # Indefinite length
            if $bytes === Whatever {
                # Peek and see if there are any chunks
                if $cbor.read-uint8($pos) == CBOR_SVal +| CBOR_Indefinite_Break {
                    "''_"
                }
                else {
                    my @chunks;
                    until (my $chunk := cbor-diagnostic($cbor, $pos, :breakable))
                          =:= Break {
                        @chunks.push($chunk);
                    }
                    '(_ ' ~ @chunks.join(', ') ~ ')';
                }
            }
            # Definite length
            elsif $bytes > CBOR_Max_UInt_63Bit {
                "'Unreasonably long byte string, length $bytes'"
            }
            else {
                my $buf = $cbor.subbuf($pos, $bytes);
                if $buf.bytes == $bytes {
                    $pos += $bytes;
                    "h'" ~ $buf.list.map(*.fmt('%02x')).join ~ "'"
                }
                else {
                    "'Byte string too short, {$buf.bytes} < $bytes bytes'"
                }
            }
        }
        when CBOR_TStr {
            my $bytes = read-uint;

            # Indefinite length
            if $bytes === Whatever {
                # Peek and see if there are any chunks
                if $cbor.read-uint8($pos) == CBOR_SVal +| CBOR_Indefinite_Break {
                    '""_'
                }
                else {
                    my @chunks;
                    until (my $chunk := cbor-diagnostic($cbor, $pos, :breakable))
                          =:= Break {
                        @chunks.push($chunk);
                    }
                    '(_ ' ~ @chunks.join(', ') ~ ')';
                }
            }
            # Definite length
            elsif $bytes > CBOR_Max_UInt_63Bit {
                "\"Unreasonably long text string, length $bytes\""
            }
            else {
                my $utf8 = $cbor.subbuf($pos, $bytes);
                if $utf8.bytes == $bytes {
                    $pos += $bytes;
                    # XXXX: JSON escaping?
                    '"' ~ $utf8.decode ~ '"'
                }
                else {
                    "\"Text string too short, {$utf8.bytes} < $bytes bytes\""
                }
            }
        }
        when CBOR_Array {
            # Indefinite length
            $argument == CBOR_Indefinite_Break
            ?? do {
                my @array;
                until (my $item := cbor-diagnostic($cbor, $pos, :breakable)) =:= Break {
                    @array.push($item);
                }
                '[_ ' ~ @array.join(', ') ~ ']'
            }
            !! '[' ~ (^read-uint).map({ cbor-diagnostic($cbor, $pos) }).join(', ') ~ ']'
        }
        when CBOR_Map {
            my @pairs;
            # Indefinite length
            if $argument == CBOR_Indefinite_Break {
                loop {
                    my $k := cbor-diagnostic($cbor, $pos, :breakable);
                    last if $k =:= Break;
                    @pairs.push($k => cbor-diagnostic($cbor, $pos));
                }
                '{_ ' ~ @pairs.fmt('%s: %s', ', ') ~ '}'
            }
            # Definite length
            else {
                for ^(read-uint) {
                    my $k = cbor-diagnostic($cbor, $pos);
                    my $v = cbor-diagnostic($cbor, $pos);
                    @pairs.push($k => $v);
                }
                '{' ~ @pairs.fmt('%s: %s', ', ') ~ '}'
            }
        }
        when CBOR_Tag {
            read-uint() ~ '(' ~ cbor-diagnostic($cbor, $pos) ~ ')'
        }
        when CBOR_SVal {
            my constant %svals = 20 => 'false', 21 => 'true',
                                 22 => 'null',  23 => 'undefined';
            sub JS-Num($v) {
                $v.isNaN   ??  'NaN'      !!
                $v ==  Inf ??  'Infinity' !!
                $v == -Inf ?? '-Infinity' !! do {
                    my $basic = (~$v).subst(/'e0'$/, '').subst(/'e'('+'|'-')'0'/, {"e$0"});
                    $basic.contains('.') ?? $basic !! $basic.subst(/('e' | $)/, {".0$0"})
                }
            }

            if $argument < CBOR_False {
                "simple($argument)"
            }
            elsif $argument <= CBOR_Undef {
                %svals{$argument}
            }
            elsif $argument == CBOR_1Byte {
                my $sval = $cbor.read-uint8($pos++);
                "simple($sval)"
            }
            elsif $argument == CBOR_2Byte {
                my $v = num-from-bin16($cbor.read-uint16($pos, BigEndian));
                $pos += 2;
                JS-Num($v) ~ '_1'
            }
            elsif $argument == CBOR_4Byte {
                my $v = $cbor.read-num32($pos, BigEndian);
                $pos += 4;
                JS-Num($v) ~ '_2'
            }
            elsif $argument == CBOR_8Byte {
                my $v = $cbor.read-num64($pos, BigEndian);
                $pos += 8;
                JS-Num($v) ~ '_3'
            }
            elsif $breakable && $argument == CBOR_Indefinite_Break {
                Break
            }
            else {
                "simple($argument)"
            }

        }
    }
}


=begin pod

=head1 NAME

CBOR::Simple - Simple codec for the CBOR serialization format


=head1 SYNOPSIS

=begin code :lang<raku>

use CBOR::Simple;

# Encode a Raku value to CBOR, or vice-versa
my $cbor = cbor-encode($value);
my $val1 = cbor-decode($cbor);               # Fails if more data past first decoded value
my $val2 = cbor-decode($cbor, my $pos = 0);  # Updates $pos after decoding first value

# By default, cbor-decode() marks partially corrupt parsed structures with
# Failure nodes at the point of corruption
my $bad  = cbor-decode(buf8.new(0x81 xx 3));  # [[[Failure]]]

# Callers can instead force throwing exceptions on any error
my $*CBOR_SIMPLE_FATAL_ERRORS = True;
my $bad  = cbor-decode(buf8.new(0x81 xx 3));  # BOOM!

# Decode CBOR into diagnostic text, used for checking encodings and complex structures
my $diag = cbor-diagnostic($cbor);

# Force the encoder to tag a value with a particular tag number
my $tagged = CBOR::Simple::Tagged.new(:$tag-number, :$value);
my $cbor   = cbor-encode($tagged);

=end code


=head1 DESCRIPTION

C<CBOR::Simple> is an easy-to-use implementation of the core functionality of
the L<CBOR serialization format|https://cbor.io/>, implementing the standard as
of L<RFC 8949|https://tools.ietf.org/html/rfc8949>, plus a collection of common
tag extensions as described below in
L<TAG IMPLEMENTATION STATUS|#tag-implementation-status>.


=head2 PERFORMANCE

C<CBOR::Simple> is one of the fastest data structure serialization codecs
available for Raku.  It is comparable in round-trip speed to C<JSON::Fast>
for data structures that are the most JSON-friendly.  For all other cases
tested, C<CBOR::Simple> produces smaller, higher fidelity encodings, faster.
For more detail, and comparison with other Raku serialization codecs, see
L<serializer-perf|https://github.com/japhb/serializer-perf>.


=head2 NYI

Currently known NOT to work:

=item Any tag marked '✘' (valid but not yet supported) or 'D' (deprecated spec)
      in the ENCODE or DECODE column of the Tag Status Details table below, or
      any tag not explicitly listed therein, will be treated as an opaque tagged
      value rather than treated as a native type.

=item Packed arrays of 128-bit floats (num128); these are not supported in
      Rakudo yet.

=item Encoding I<finite> 16-bit floats (num16); encoding 16-bit NaN and ±Inf,
      as well as decoding any num16 all work.  This is a performance tradeoff
      rather than a technical limitation; detecting whether a finite num32 can
      be shrunk to 16 bits without losing information is costly and rarely
      results in space savings except in trivial cases (e.g. Nums containing
      only small integers).


=head2 TAG CONTENT STRICTNESS

When encoding, C<CBOR::Simple> makes every attempt to encode tagged content
strictly within the tag standards as written, always producing spec-compliant
encoded values.

When decoding, C<CBOR::Simple> will often slightly relax the allowed content
types in tagged content, especially when later tag proposals made no change
other than to extend the allowed content types and allocate a new tag number
for that.  In the extension case C<CBOR::Simple> is likely to allow both the
old and new tag to accept the same content domain when decoding.

For example, when encoding C<CBOR::Simple> will always encode C<Instant> or
C<DateTime> as a CBOR epoch-based date/time (tag 1), using standard integer
or floating point content data.  But when I<decoding>, C<CBOR::Simple> will
accept any content that decodes properly as a Raku C<Real> value -- and in
particular will handle a CBOR Rational (tag 30) as another valid content type.


=head2 DATE, DATETIME, INSTANT

Raku's builtin time handling is richer than the default CBOR data model (though
certain tag extensions improve this), so the following mappings apply:

=item1 Encoding

=item2 C<Instant> and C<DateTime> are both written as tag 1 (epoch-based
       date/time) with integer (if lossless) or floating point content.

=item2 Other C<Dateish> are written as tag 100 (RFC 8943 days since 1970-01-01).

=item1 Decoding

=item2 Tag 0 (date/time string) is parsed as a C<DateTime>.

=item2 Tag 1 (epoch-based date/time) is parsed via C<Instant.from-posix()>, and
       handles any Real type in the tag content.

=item2 Tag 100 (days since 1970-01-01) is parsed via C<Date.new-from-daycount()>.

=item2 Tag 1004 (date string) is parsed as a C<Date>.


=head2 UNDEFINED VALUES

=item CBOR's C<null> is translated as C<Any> in Raku.

=item CBOR's C<undefined> is translated as C<Mu> in Raku.

=item A real C<Nil> in an array (which must be I<bound>, not assigned) is
      encoded as a CBOR Absent tag (31).  Absent values will be recognized on
      decode as well, but since array contents are I<assigned> into their
      parent array during decoding, a C<Nil> in an array will be translated to
      C<Any> by Raku's array assignment semantics.


=head2 OTHER SPECIAL CASES

=item To mark a substructure for lazy decoding (treating it as an opaque
      C<Blob> until explicitly decoded), use the tagged value idiom in the
      SYNOPSIS with `:tag-number(24)` (encoded CBOR value) or
      `:tag-number(63)` (encoded CBOR Sequence).

=item CBOR strings claiming to be longer than C<2⁶³-1> are treated as malformed.

=item Bigfloats and decimal fractions (tags 4, 5, 264, 265) with very large
      exponents may result in numeric overflow when decoded.

=item Keys for Associative types are sorted using Raku's internal `sort` method
      rather than the RFC 8949 default sort, because the latter is much slower.

=item C<cbor-diagnostic()> always adds encoding indicators for float values.


=head2 TAG IMPLEMENTATION STATUS

Note that unrecognized tags will decode to their contents wrapped with a
C<CBOR::Simple::Tagged> object that records its C<tag-number>; check marks in
the details table indicate conversion to/from an appropriate native Raku type
rather than this default behavior.

=begin table :caption<Tag Status Overview: Native Raku Types>
    GROUP          | SUPPORT | NOTES
    ============== |=========|======
    Core           | Good    | Core RFC 8949 CBOR data model and syntax
    Collections    | Good    | Sets, maps with only object or only string keys
    Graph          | NONE    | Cyclic, indirected, and self-referential structures
    Numbers        | Good    | Rational/BigInt/BigFloat support except non-finite triplets
    Packed Arrays  | Partial | Packed num32/64, [u]int* arrays full; num16 is read-only
    Special Arrays | NONE    | Explicit multi-dim/homogenous arrays
    Tag Fallbacks  | Good    | Round tripping of unknown tagged content
    Date/Time      | Partial | All but tagged time (tags 1001-1003) supported
=end table

=begin table :caption<Tag Status Overview: Specialty Types>
    GROUP          | SUPPORT | NOTES
    ============== |=========|======
    Encodings      | NONE    | baseN, MIME, YANG, BER, non-UTF-8 strings
    Geo            | NONE    | Geographic coordinates and shapes
    Identifiers    | NONE    | URI, IRI, UUID, IPLD CID, general identifiers
    Networking     | NONE    | IPv4/IPv6 addresses, subnets, and masks
    Security       | NONE    | COSE and CWT
    Specialty      | NONE    | IoT data, Openswan, PlatformV, DOTS, ERIS, RAINS
    String Hints   | NONE    | JSON conversions, language tags, regex
=end table

=begin table :caption<Tag Status Details>
    SPEC         |        TAGS | ENCODE | DECODE | NOTES
    =============|=============|========|========|===================================
    RFC 8949     |           0 | →      | ✓      | DateTime strings → Encoded as tag 1
    RFC 8949     |           1 | ✓      | ✓      | DateTime/Instant
    RFC 8949     |         2,3 | ✓      | ✓      | (Big) Int
    RFC 8949     |         4,5 | →      | ✓      | Big fractions → Encoded as tag 30
    unassigned   |        6-15 |        |        |
    COSE         |       16-18 | ✘      | ✘      | MAC/Signatures
    unassigned   |       19-20 |        |        |
    RFC 8949     |       21-23 | ✘      | ✘      | Expected JSON conversion to baseN
    RFC 8949     |          24 | T      | ✓      | Encoded CBOR data item
    [Lehmann]    |          25 | ✘      | ✘      | String backrefs
    [Lehmann]    |       26,27 | ✘      | ✘      | General serialized objects
    [Lehmann]    |       28,29 | ✘      | ✘      | Shareable referenced values
    [Occil]      |          30 | ✓      | ✓      | Rational numbers
    [Vaarala]    |          31 | ✓      | *      | Absent values
    RFC 8949     |       32-34 | ✘      | ✘      | URIs and base64 encoding
    RFC 7094     |          35 | D      | D      | PCRE/ECMA 262 regex (DEPRECATED)
    RFC 8949     |          36 | ✘      | ✘      | Text-based MIME message
    [Clemente]   |          37 | ✘      | ✘      | Binary UUID
    [Occil]      |          38 | ✘      | ✘      | Language-tagged string
    [Clemente]   |          39 | ✘      | ✘      | Identifier semantics
    RFC 8746     |          40 | ✘      | ✘      | Row-major multidim array
    RFC 8746     |          41 | ✘      | ✘      | Homogenous array
    [Mische]     |          42 | ✘      | ✘      | IPLD content identifier
    [YANG]       |       43-47 | ✘      | ✘      | YANG datatypes
    unassigned   |       48-51 |        |        |
    draft        |          52 | D      | D      | IPv4 address/network (DEPRECATED)
    unassigned   |          53 |        |        |
    draft        |          54 | D      | D      | IPv6 address/network (DEPRECATED)
    unassigned   |       55-60 |        |        |
    RFC 8392     |          61 | ✘      | ✘      | CBOR Web Token (CWT)
    unassigned   |          62 |        |        |
    [Bormann]    |          63 | T      | ✓      | Encoded CBOR Sequence
    RFC 8746     |       64-79 | ✓      | ✓      | Packed int arrays
    RFC 8746     |       80-87 | *      | ✓      | Packed num arrays (except 128-bit)
    unassigned   |       88-95 |        |        |
    COSE         |       96-98 | ✘      | ✘      | Encryption/MAC/Signatures
    unassigned   |          99 |        |        |
    RFC 8943     |         100 | ✓      | ✓      | Date
    unassigned   |     101-102 |        |        |
    [Vidovic]    |         103 | ✘      | ✘      | Geo coords
    [Clarke]     |         104 | ✘      | ✘      | Geo coords ref system WKT/EPSG
    unassigned   |     105-109 |        |        |
    RFC 9090     |     110-112 | ✘      | ✘      | BER-encoded object ID
    unassigned   |     113-119 |        |        |
    [Vidovic]    |         120 | ✘      | ✘      | IoT data point
    unassigned   |     121-255 |        |        |
    [Lehmann]    |         256 | ✘      | ✘      | String backrefs (see tag 25)
    [Occil]      |         257 | ✘      | ✘      | Binary MIME message
    [Napoli]     |         258 | ✓      | ✓      | Set
    [Holloway]   |         259 | T      | ✓      | Map with object keys
    [Raju]       |     260-261 | ✘      | ✘      | IPv4/IPv6/MAC address/network
    [Raju]       |     262-263 | ✘      | ✘      | Embedded JSON/hex strings
    [Occil]      |     264-265 | →      | *      | Extended fractions -> Encoded as tag 30
    [Occil]      |     266-267 | ✘      | ✘      | IRI/IRI reference
    [Occil]      |     268-270 | ✘✘     | ✘✘     | Triplet non-finite numerics
    RFC 9132     |         271 | ✘✘     | ✘✘     | DDoS Open Threat Signaling (DOTS)
    [Vaarala]    |     272-274 | ✘      | ✘      | Non-UTF-8 strings
    [Cormier]    |         275 | T      | ✓      | Map with only string keys
    [ERIS]       |         276 | ✘      | ✘      | ERIS binary read capability
    [Meins]      |     277-278 | ✘      | ✘      | Geo area shape/velocity
    unassigned   |    279-1000 |        |        |
    [Bormann]    |   1001-1003 | ✘      | ✘      | Extended time representations
    RFC 8943     |        1004 | →      | ✓      | → Encoded as tag 100
    unassigned   |   1005-1039 |        |        |
    RFC 8746     |        1040 | ✘      | ✘      | Column-major multidim array
    unassigned   |  1041-22097 |        |        |
    [Lehmann]    |       22098 | ✘      | ✘      | Hint for additional indirection
    unassigned   | 22099-25440 |        |        |
    [Broadwell]  | 25441       | ✓      | ✓      | Capture: reference implementation
    unassigned   | 25442-49999 |        |        |
    [Tongzhou]   | 50000-50011 | ✘✘     | ✘✘     | PlatformV
    unassigned   | 50012-55798 |        |        |
    RFC 8949     |       55799 | ✓      | ✓      | Self-described CBOR
    [Richardson] |       55800 | ✓      | ✓      | Self-described CBOR Sequence
    unassigned   | 55801-65534 |        |        |
    invalid      |       65535 |        | ✓      | Invalid tag detected
    unassigned   | 65536-15309735 |     |        |
    [Trammell]   |    15309736 | ✘✘     | ✘✘     | RAINS message
    unassigned   | 15309737-1330664269  |  |     |
    [Hussain]    |  1330664270 | ✘✘     | ✘✘     | CBOR-encoded Openswan config file
    unassigned   | 1330664271-4294967294 |  |    |
    invalid      |  4294967295 |        | ✓      | Invalid tag detected
    unassigned   |         ... |        |        |
    invalid      | 18446744073709551615 |  | ✓   | Invalid tag detected
=end table

=begin table :caption<Tag Table Symbol Key>
    SYMBOL | MEANING
    =======|========
    ✓      | Fully supported
    *      | Supported, but see notes above
    T      | Encoding supported by explicitly tagging contents
    →      | Raku values will be encoded using a different tag
    D      | Deprecated and unsupported tag spec; may eventually be decodable
    ✘      | Not yet implemented
    ✘!     | Not yet implemented, but already requested
    ✘?     | Not yet implemented, but may be easy to add
    ✘✘     | Probably won't be implemented in CBOR::Simple
=end table


=head1 AUTHOR

Geoffrey Broadwell <gjb@sonic.net>


=head1 COPYRIGHT AND LICENSE

Copyright 2021,2025 Geoffrey Broadwell

This library is free software; you can redistribute it and/or modify it under
the Artistic License 2.0.

=end pod
