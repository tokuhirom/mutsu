use Test;

# From vCard::Parser's Actions: `method m($/) { make $/.subst(/ .. /, '', :g) }`.
# A method call on `$/` must not overwrite a `$/` parameter with the `$/`
# that the nested `.subst` produced.

plan 5;

sub keeps-match($/) {
    my $r = $/.subst(/ a /, "X", :g);
    ($r, $/.WHAT)
}

"banana" ~~ /nan/;
my ($r, $type) = keeps-match($/);
is $r, "nXn", '$/.subst(:g) result';
is $type.gist, '(Match)', '$/ parameter is still the Match after .subst(:g)';

sub keeps-match-single($/) {
    my $r = $/.subst(/ n /, "Q");
    $/.Str
}
"banana" ~~ /nan/;
is keeps-match-single($/), "nan", '$/ parameter survives non-global .subst';

class A {
    method m($/) { $/.subst(/ \\ )> <[,;]> /, Q{}, :g).subst(/ \\n/, "\n", :g) }
}
"a\\,b" ~~ /.+/;
is A.new.m($/), "a,b", 'chained subst on $/ in a method';

"xyz" ~~ /y/;
"abc".subst(/b/, "X", :g);
is $/.WHAT.gist, '(List)', 'a Str .subst(:g) still sets the ambient $/ to a List';
