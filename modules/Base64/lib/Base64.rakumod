unit module Base64;

my Str @chars64base = flat 'A'..'Z','a'..'z','0'..'9';
my Str @chars64std  = flat @chars64base, '+', '/';
my Str @chars64uri  = flat @chars64base, '-', '_';


our proto sub encode-base64(|) is export {*}
multi sub encode-base64(Bool:D :$pad!, |c)                    { samewith(:pad(?$pad ?? '=' !! ''), |c) }
multi sub encode-base64(Bool:D :$uri! where *.so, |c)         { samewith(:alpha(@chars64uri), |c)      }
multi sub encode-base64(Bool:D :$str! where *.so, |c --> Str) { samewith(|c).join                      }
multi sub encode-base64(Str:D  $str, |c)                      { samewith(Blob.new($str.ords), |c)      }
multi sub encode-base64(
    Blob:D $blob,
    Str:D :$pad where *.chars <= 1 = '=',
    Str:D :@alpha                  = @chars64std,
--> Seq) {
    grep *.so, $blob.rotor(3, :partial).map: -> $chunk {
        my $padding = 0;
        my $n := [+] $chunk.pairs.map: -> $c {
            LAST { $padding = !$pad ?? 0 !! do with (3 - ($c.key+1) % 3) { $^a == 3 ?? 0 !! $^a } }
            $c.value +< ((state $m = 24) -= 8)
        }
        my $res := (18, 12, 6, 0).map({ $n +> $_ +& 63 });
        (slip(@alpha[$res[*]][0..*-($padding ?? $padding+1 !! 0)]),
            ((^$padding).map({"$pad"}).Slip if $padding)).Slip
    }
}


our proto sub decode-base64(|) is export {*}
multi sub decode-base64(Bool:D :$uri! where *.so, |c)                 { samewith(:alpha(@chars64uri), |c)   }
multi sub decode-base64(Bool:D :buf(:$bin)! where *.so, |c --> Buf)   { Buf.new(samewith(|c)) || Buf.new(0) }
multi sub decode-base64(Blob:D $blob, |c)                             { samewith($blob.decode, |c)          }
multi sub decode-base64(
    Str:D $str,
    Str:D :@alpha = @chars64std
--> Seq) {
    $str.comb(/@alpha/).rotor(4, :partial).map: -> $chunk {
        state %lookup = @alpha.kv.hash.antipairs;
        my $n   = [+] $chunk.map: { (%lookup{$_} || 0) +< ((state $m = 24) -= 6) }
        my $res = (16, 8, 0).map: { $n +> $_ +& 255 }
        slip($res.head( 3 - ( 4 - $chunk.elems ) ) )
    }
}
