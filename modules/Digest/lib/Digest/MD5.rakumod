unit module Digest::MD5;

proto md5($msg) returns Blob is export {*}
multi md5(Str $msg) { md5 $msg.encode }
multi md5(Blob $msg) {
  my \bits = 8 * $msg.elems;
  my buf8 $buf .= new;
  $buf.write-uint32: $buf.elems, $_, LittleEndian for
    reduce -> Blob $blob, blob32 $X {
      blob32.new: $blob Z+
        reduce -> $b, $i {
          blob32.new: 
            $b[3],
            $b[1] + 
              -> uint32 \x, \n { (x +< n) +| (x +> (32-n)) }(
              ($b[0] + (BEGIN Array.new:
              { ($^x +& $^y) +| (+^$x +& $^z) },
              { ($^x +& $^z) +| ($^y +& +^$z) },
              { $^x +^ $^y +^ $^z },
              { $^y +^ ($^x +| +^$^z) }
              )[$i div 16](|$b[1..3]) +
              (BEGIN blob32.new: map &floor ∘ * * 2**32 ∘ &abs ∘ &sin ∘ * + 1, ^64)[$i] +
              $X[(BEGIN Blob.new: 16 X[R%] flat ($++, 5*$++ + 1, 3*$++ + 5, 7*$++) Xxx 16)[$i]]
	      ) mod 2**32,
              (BEGIN flat < 7 12 17 22 5 9 14 20 4 11 16 23 6 10 15 21 >.rotor(4) Xxx 4)[$i]
            ),
            $b[1],
            $b[2]
        }, $blob, |^64;
    },
    (BEGIN blob32.new: 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476),
    |map { blob32.new: @$_ },
      {
	$^b.push($_) for (@$msg, 0x80, 0x00 xx (-(bits div 8 + 1 + 8) % 64))
	    .flat.rotor(4).map({ :256[@^a.reverse] });
	$b.write-uint64: $b.elems, bits, LittleEndian;
	$b;
      }(buf32.new)
    .rotor(16);
    $buf;
}

# vi: ft=raku
