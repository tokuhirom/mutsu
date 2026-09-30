# A `sub EXPORT` module in the French/lizmat "hand-built Map" shape, distinct
# from the `UNIT::`-grep idiom `RuntimeExport/RuntimeExportListop.rakumod`
# pins: every exported name is a LOCAL declaration inside the hook's own
# body, not drawn from the compunit's unit scope. The importer's parse has to
# learn both kinds: `&infix:<et>`/`&infix:<ou>` because an undeclared word is
# not an infix ("Two terms in a row", #9918), and `vrai`/`faux` because an
# unknown bareword defaults to a listop-call head.
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
