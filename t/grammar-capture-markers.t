use Test;

plan 10;

# `my grammar { ... }` / `my class { ... }` as an *expression* (anonymous lexical
# package declarator), e.g. `my grammar { ... }.parse: $s`.
{
    my $r = my grammar { token TOP { \d+ } }.parse("123");
    isa-ok $r, Match, 'my grammar {...}.parse returns a Match';
    is ~$r, '123', 'my grammar {...}.parse matched text';
}
{
    my $o = my class { method greet { "hi" } }.new;
    is $o.greet, 'hi', 'my class {...}.new works as an expression';
}

# Capture markers `<(` / `)>` inside grammar tokens restrict the token's submatch.
constant $s = '1234567890foobarMEOW';
{
    my $r = my grammar {
        token TOP { <foo><bar><ber> }
        token foo { 12345 <( 67890 }
        token bar { foo   )> bar   }
        token ber { M <(EO)>  W    }
    }.parse: $s;
    isa-ok $r, Match, 'grammar with capture markers parses';
    is ~$r<foo>, '67890', '<( restricts <foo> start';
    is ~$r<bar>, 'foo',   ')> restricts <bar> end';
    is ~$r<ber>, 'EO',    '<( ... )> restricts <ber> on both sides';
}
{
    my $r = my grammar {
        token TOP { <foo><bar><ber> }
        token foo { \d**5 <( \d+ }
        token bar { <:lower>**3 )> <:lower>+ }
        token ber { <:upper> <(.**2)> .+  }
    }.parse: $s;
    is ~$r<foo>, '67890', '<( with \d (grammar 2)';
    is ~$r<bar>, 'foo',   ')> with <:lower> (grammar 2)';
    is ~$r<ber>, 'EO',    '<( )> with <:upper> (grammar 2)';
}
