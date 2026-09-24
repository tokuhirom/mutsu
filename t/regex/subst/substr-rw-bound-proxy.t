use Test;

# A `substr-rw` result bound with `:=` is a Proxy whose STORE splices into the
# source string (#9200). Both the method form and the sub form.

plan 9;

{
    my $s = "pqab";
    my $r := $s.substr-rw(1, 1);
    $r = "Y";
    is $s, 'pYab', 'method form: store through the bound Proxy';
}

{
    my $s = "pqab";
    my $r := substr-rw($s, 1, 1);
    $r = "Y";
    is $s, 'pYab', 'sub form: store through the bound Proxy';
}

{
    my $s = "pqab";
    my $r := $s.substr-rw(1, 2);
    is $r, 'qa', 'the bound Proxy fetches the substring';
    $s = "WXYZ";
    is $r, 'XY', '... and re-fetches after the source changes';
}

{
    my $s = "pqab";
    my $r := $s.substr-rw(1, 1);
    $r ~= "!";
    is $s, 'pq!ab', 'a read-modify-write through the bound Proxy';
}

{
    my $s = "pqab";
    sub f { my $r := $s.substr-rw(2, 1); $r = "Q" }
    f();
    is $s, 'pqQb', 'bound inside a routine, writing a captured outer string';
}

{
    my $s = "pqab";
    my $x = $s.substr-rw(0, 1);
    $x = "Z";
    is $s, 'pqab', 'plain assignment copies the value; the source is untouched';
    is $s.substr-rw(1, 2).uc, 'QA', 'a method on the unbound result sees the substring';
}

{
    my $s = "pqab";
    $s.substr-rw(1, 1) = "Y";
    is $s, 'pYab', 'the direct assignment form still works';
}
