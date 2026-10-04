my constant HIDDEN = 7;
constant $bare-pat = 'xyz';

class ToplevelMarkersBare {
    method matches($s) { so $s ~~ /$bare-pat/ }
    method hidden() { HIDDEN }
    method shadowed($s) { my $bare-pat = 'q'; so $s ~~ /$bare-pat/ }
}
