use Test;

# From the C::Parser distribution: a `#` comment inside a `rule` body that ends
# in `?`, `*` or `+` must stay a comment. Sigspace insertion used to read its
# last character as a quantifier on the previous term and swallow the newline.

plan 6;

grammar G {
    rule a {
        { }
        # maybe this could be const/final?
        :my @*B = (1, 2);
        <ident>
    }
    rule b { # trailing star *
        <ident> }
    rule c { <ident> # trailing plus +
        ';' }
    rule d { <ident>?
        # x?
        ';' }
}

is G.subparse("foo", :rule<a>).pos, 3, 'comment ending in ? before :my';
is G.subparse("foo", :rule<b>).pos, 3, 'comment ending in *';
is G.subparse("foo;", :rule<c>).pos, 4, 'comment ending in + after an atom';
is G.subparse("foo ;", :rule<d>).pos, 5, 'comment after a quantified atom';
ok G.subparse("foo", :rule<a>).defined, 'match object is defined';
is G.subparse("foo", :rule<b>).Str, "foo", 'matched text';
