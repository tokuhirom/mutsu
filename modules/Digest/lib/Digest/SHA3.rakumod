#!/usr/bin/env raku
unit module Digest::SHA3;

our proto sha3_224($) is export {*}
our proto sha3_256($) is export {*}
our proto sha3_384($) is export {*}
our proto sha3_512($) is export {*}
our proto shake128($, $) is export {*}
our proto shake256($, $) is export {*}

multi sha3_224(Str $str) { samewith $str.encode }
multi sha3_256(Str $str) { samewith $str.encode }
multi sha3_384(Str $str) { samewith $str.encode }
multi sha3_512(Str $str) { samewith $str.encode }
multi shake128(Str $str, UInt $n) { samewith $str.encode, $n }
multi shake128(Str $str, Whatever) { samewith $str.encode, * }
multi shake256(Str $str, UInt $n) { samewith $str.encode, $n }
multi shake256(Str $str, Whatever) { samewith $str.encode, * }

multi sha3_224(Blob $input) { [~] Keccak $input, delimitedSuffix => 0x06, outputByteLen => 224 div 8, rate => 1152, capacity => 448 }
multi sha3_256(Blob $input) { [~] Keccak $input, delimitedSuffix => 0x06, outputByteLen => 256 div 8, rate => 1088, capacity => 512 }
multi sha3_384(Blob $input) { [~] Keccak $input, delimitedSuffix => 0x06, outputByteLen => 384 div 8, rate =>  832, capacity => 768 }
multi sha3_512(Blob $input) { [~] Keccak $input, delimitedSuffix => 0x06, outputByteLen => 512 div 8, rate =>  576, capacity => 1024 }
multi shake128(Blob $input, UInt $outputByteLen) { Keccak $input, delimitedSuffix => 0x1F, :$outputByteLen, rate => 1344, capacity => 256 }
multi shake128(Blob $input, Whatever)            { Keccak $input, delimitedSuffix => 0x1F,                  rate => 1344, capacity => 256 }
multi shake256(Blob $input, UInt $outputByteLen) { Keccak $input, delimitedSuffix => 0x1F, :$outputByteLen, rate => 1088, capacity => 512 }
multi shake256(Blob $input, Whatever)            { Keccak $input, delimitedSuffix => 0x1F,                  rate => 1088, capacity => 512 }

=for CREDITS
The following is a straight-forward translation of
L<https://github.com/XKCP/XKCP/blob/master/Standalone/CompactFIPS202/Python/CompactFIPS202.py>

sub ROL64 { ($^a +> (64 - $_) +| $a +< $_) % (1 +< 64) given $^n%64 }

multi KeccakF1600(@lanes) {
  my $R = 1;
  for ^24 {
    # θ
    my @C = map -> $x { [+^] @lanes[$x;^5] }, ^5;
    my @D = map -> $x { @C[($x-1)%5] +^ ROL64(@C[($x+1)%5], 1) }, ^5; 
    for ^5 X ^5 -> ($x, $y) { @lanes[$x;$y] +^= @D[$x] }
    # ρ and π
    my ($x, $y) = 1, 0;
    my $current = @lanes[$x; $y];
    for ^24 -> $t {
      ($x, $y) = $y, (2*$x+3*$y)%5;
      ($current, @lanes[$x;$y]) =
      @lanes[$x;$y], ROL64 $current, ($t+1)*($t+2) div 2;
    }
    # χ
    for ^5 -> $y {
      my @T = map { @lanes[$_;$y] }, ^5;
      for ^5 -> $x {
        @lanes[$x;$y] = @T[$x] +^ (
	  +^@T[($x+1)%5] +& @T[($x+2)%5]
	);
      }
    }
    # ι
    for ^7 -> $j {
      $R = ($R +< 1 +^ (($R +> 7)*0x71)) % 256;
      if $R +& 2 { @lanes[0;0] +^= 1 +< ((1 +< $j) - 1); }
    }
  }
  return @lanes;
}

multi KeccakF1600(blob8 $state) {
  sub load64 { :256[|@^b.reverse] }
  sub store64 { $^a.polymod: 256 xx 7 }
  my @lanes;
  for ^5 X ^5 -> ($i, $j) { @lanes[$i;$j] = load64 $state.subbuf: 8*($i + 5*$j), 8; }
  KeccakF1600 @lanes;
  my buf8 $new-state .= new: 0 xx 200;
  for ^5 X ^5 -> ($i, $j) {
    given 8*($i + 5*$j) {
      $new-state[$_ ..^ $_ + 8] = store64 @lanes[$i;$j];
    }
  }
  return $new-state;
}

our proto Keccak(
  Blob $inputBytes,
  byte :$delimitedSuffix,
  UInt :$outputByteLen is copy,
  UInt :$rate where * %% 8,
  UInt :$capacity where $rate + $capacity == 1600,
) {*}

multi Keccak(
  $inputBytes,
  :$delimitedSuffix,
  :$rate,
  :$capacity
) {

  my buf8 $outputBytes .= new;
  my buf8 $state .= new: 0 xx 200;
  my $rateInBytes = $rate div 8;
  my $blockSize = 0;
  my $inputOffset = 0;

  # === Absorb all the input blocks ===
  while $inputOffset < $inputBytes.elems {
    $blockSize = min $inputBytes - $inputOffset, $rateInBytes;
    for ^$blockSize -> $i {
      $state[$i] +^= $inputBytes[$i+$inputOffset];
    }
    $inputOffset += $blockSize;
    if $blockSize == $rateInBytes {
      $state .= &KeccakF1600;
      $blockSize = 0;
    }
  }

  # === Do the padding and switch to the squeezing phase ===
  $state[$blockSize] +^= $delimitedSuffix;
  if ($delimitedSuffix +& 0x80) != 0 and ($blockSize == ($rateInBytes-1)) {
    $state .= &KeccakF1600;
  }
  $state[$rateInBytes-1] +^= 0x80;
  $state .= &KeccakF1600;
  
  # === Squeeze out all the output blocks ===
  gather loop {
    take $state.subbuf: 0, $rateInBytes;
    $state .= &KeccakF1600;
  }

}

multi Keccak(
  $inputBytes,
  :$delimitedSuffix,
  :$outputByteLen is copy,
  :$rate,
  :$capacity,
) {
  gather for samewith $inputBytes, :$delimitedSuffix, :$rate, :$capacity {
    # === Squeeze out all the output blocks ===
    my $blockSize = min $outputByteLen, .elems;
    take .subbuf: 0, $blockSize;
    $outputByteLen -= $blockSize;
    last if $outputByteLen ≤ 0;
  }
}

# vim: ft=raku
