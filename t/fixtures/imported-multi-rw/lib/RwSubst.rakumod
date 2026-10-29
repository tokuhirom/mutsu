unit module RwSubst;

proto sub fetch(|) is export {*}
multi sub fetch(Str:D \s, Int:D $o, Int:D $l is copy, Str() $r --> Str:D) {
    $l = s.chars + $l if $l < 0;
    my $u := s.substr($o, $l);
    s.substr-rw($o, $l) = $r;
    $u
}
multi sub fetch(Str:D \s, Int:D $o is copy, Int:D $l is copy = s.chars - $o) is rw {
    $l = s.chars + $l if $l < 0;
    Proxy.new(
      FETCH => -> $ { s.substr($o, $l) },
      STORE => -> $, \new { s.substr-rw($o, $l) = new },
    )
}

multi sub size-of($s) is export is rw { $s.chars }
