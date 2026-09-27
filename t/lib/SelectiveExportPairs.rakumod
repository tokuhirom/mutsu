# Fixture for t/modules/import-export/export-sub-selective-lexical-multi.t: String::Utils'
# EXPORT hands back `UNIT::{"&$_"}:p` pairs, whose values sit in the stash
# element's container rather than being the bare Sub.
my sub between(str $s, str $l, str $r) {
    my $from = $s.index($l);
    return Nil without $from;
    $from += $l.chars;
    my $to = $s.index($r, $from);
    $to.defined ?? $s.substr($from, $to - $from) !! Nil
}

my sub EXPORT(*@names) {
    Map.new: @names.map: { UNIT::{"&$_"}:p if UNIT::{"&$_"}:exists }
}
