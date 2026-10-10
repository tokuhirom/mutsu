class RegexDeclClassLexical {
    my %e = P => 1, D => 2;
    my $expression = %e.keys.sort.join('|');
    my regex fe { '%'$<specifier>=[<$expression>] }
    sub fmt(Str $f) is export(:FORMAT) {
        $f.subst(&fe, -> ( :$specifier ) { "<$specifier>" }, :g)
    }
    sub fmt-match(Str $f) is export(:FORMAT) { $f.match(&fe).Str }
}
