# A `sub EXPORT` module in the French/lizmat "hand-built Map" shape, distinct
# from the `UNIT::`-grep idiom `RuntimeExport/RuntimeExportListop.rakumod`
# pins: every exported name is a LOCAL declaration inside the hook's own
# body, not drawn from the compunit's unit scope. `&infix:<et>`/`&infix:<ou>`
# need no special parse-time help (a custom infix word is accepted
# speculatively and resolved at run time regardless of whether the parser
# ever learns it is declared); `vrai`/`faux` do, because an unknown bareword
# defaults to a listop-call head and has no such fallback.
use v6.d;

sub EXPORT(|) {
    my &infix:<et> = sub ($a, $b) { $a && $b };
    my &infix:<ou> = sub ($a, $b) { $a || $b };
    my \vrai = True;
    my \faux = False;
    Map.new(
        '&infix:<et>' => &infix:<et>,
        '&infix:<ou>' => &infix:<ou>,
        'vrai'        => vrai,
        'faux'        => faux,
    );
}
