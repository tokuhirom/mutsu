unit module CatchInlineExport;

sub wrap-text(Str:D $text, Bool:D :$error = False --> Hash) is export {
    { text => $text, error => $error }
}
