#!/usr/bin/env raku
unit module Digest::SHA2;

proto sha224($) returns blob8 is export {*}
proto sha256($, :$initial-hash) returns blob8 is export {*}
proto sha384($) returns blob8 is export {*}
proto sha512($, :$initial-hash) returns blob8 is export {*}

multi sha224(Str $str) { samewith $str.encode }
multi sha256(Str $str) { samewith $str.encode }
multi sha384(Str $str) { samewith $str.encode }
multi sha512(Str $str) { samewith $str.encode }

constant @primes = grep *.is-prime, 2 .. *;
sub infix:<√>( UInt $p where * >= 2, UInt $n --> FatRat ) {
  my $N = $n*2**(64*$p);
  (exp(log($n)/$p + 64*log(2)).Int, { ( ($p-1) * $^x + $N div $x**($p-1) ) div $p } ... *)
  .first({ $_**$p ≤ $N < ($_+1)**$p })
  .FatRat / 2**64
}

sub frac(Real $x, UInt $n --> Int) { (($x - $x.floor)*2**$n).floor }
sub  Ch { $^x +& $^y +^ +^$x +& $^z }
sub Maj { $^x +& $^y +^ $x +& $^z +^ $y +& $z }

multi sha224(blob8 $data) {
  sha256(
    $data,
    initial-hash => 
      BEGIN blob32.new:
	map { frac(2√$_, 64) mod 2**32 }, @primes[8..^16]
  ).subbuf(0, 28)
}

multi sha256(
  blob8  $data,
  blob32 :$initial-hash = 
    BEGIN blob32.new: map { frac sqrt($_), 32 }, @primes[^8]
) {

  sub rotr(uint32 $n, UInt $b) { $n +> $b +| $n +< (32 - $b) }
  sub Σ0 { rotr($^x,  2) +^ rotr($x, 13) +^ rotr($x, 22) }
  sub Σ1 { rotr($^x,  6) +^ rotr($x, 11) +^ rotr($x, 25) }
  sub σ0 { rotr($^x,  7) +^ rotr($x, 18) +^ $x +>  3 }
  sub σ1 { rotr($^x, 17) +^ rotr($x, 19) +^ $x +> 10 }

  return blob8.new:
    map |*.polymod(256 xx 3).reverse,
    flat
    (
      $initial-hash,
      |blob32.new(
	(
	  @$data,
	  0x80,
	  0 xx (-($data + 1 + 8) mod 64),
	  (8*$data).polymod(256 xx 7).reverse
	).flat
	.rotor(4)
	.map: { :256[@$_] }
      ).rotor(16)
    ).reduce(
      -> $H, $block {
	blob32.new: $H[] Z+
	  reduce -> blob32 $h, $j {
	    my uint32 ($T1, $T2) =
	      $h[7] + Σ1($h[4]) + Ch(|$h[4..6])
	      + (BEGIN map { frac $_ **(1/3), 32 }, @primes[^64])[$j]
	      + (
		(state buf32 $w .= new)[$j] = $j < 16 ?? $block[$j] !!
		σ0($w[$j-15]) + $w[$j-7] + σ1($w[$j-2]) + $w[$j-16]
	      ),
	      Σ0($h[0]) + Maj(|$h[0..2]);
	    blob32.new: $T1 + $T2, |$h[^3], $h[3] + $T1, |$h[4..6];
	  }, $H, |^64;
      }
    ).list

}

multi sha384(blob8 $data) {
  sha512(
    $data,
    initial-hash => 
      BEGIN blob64.new: map { frac 2√$_, 64 }, @primes[8..^16]
  ).subbuf(0, 48)
}

multi sha512(
  blob8 $data,
  blob64 :$initial-hash = 
    BEGIN blob64.new: map { frac 2√$_, 64 }, @primes[^8]
) {
 
  sub rotr($n, $b) { $n +> $b +| $n +< (64 - $b) }
  sub Σ0 { rotr($^x, 28) +^ rotr($x, 34) +^ rotr($x, 39) }
  sub Σ1 { rotr($^x, 14) +^ rotr($x, 18) +^ rotr($x, 41) }
  sub σ0 { rotr($^x,  1) +^ rotr($x,  8) +^ $x +> 7 }
  sub σ1 { rotr($^x, 19) +^ rotr($x, 61) +^ $x +> 6 }

  blob8.new:
    map |*.polymod(256 xx 7).reverse,
    |reduce
      -> $H, $block {
	blob64.new: map * mod 2**64, (
	  $H[] Z+ reduce -> blob64 $h, UInt $t {
	    my uint64 ($T1, $T2) = map *%2**64,
	      $h[7] + Σ1($h[4]) + Ch(|$h[4..6]) +
	      (BEGIN blob64.new: map { frac 3√$_, 64 }, @primes[^80])[$t] +
	      (
		(state buf64 $w .= new)[$t] = (
		  $t < 16 ?? $block[$t] !!
		  σ0($w[$t-15]) + $w[$t-7] + σ1($w[$t-2]) + $w[$t-16];
		) % 2**64
	      ),
	      Σ0($h[0]) + Maj(|$h[0..2]);
	    blob64.new:
	      ($T1   + $T2) mod 2**64, |$h[0..2],
	      ($h[3] + $T1) mod 2**64, |$h[4..6]
	    ;
	  }, $H, |^80
	)
      },
      $initial-hash,
      |blob64.new(
	(
	  flat
	  @$data,
	  0x80,
	  0 xx (-($data + 1 + 16) mod 128),
	  (8*$data).polymod(256 xx 15).reverse;
	)
	.rotor(8)
	.map: { :256[@$_] }
      ).rotor(16)
  ;

}
