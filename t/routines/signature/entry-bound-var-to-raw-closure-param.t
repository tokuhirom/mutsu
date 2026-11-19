use Test;

# A variable `:=`-bound to a missing hash element, passed to a closure's
# `is raw` / `is rw` parameter, passes the element's location: a write through
# the parameter, or a bind below it, creates the element (#18186).

plan 7;

{
    my %h; my $c := %h<a>;
    (-> $h is raw { $h = 5 })($c);
    is-deeply %h, %(a => 5), 'is raw: a write through the parameter';
}

{
    my %g; my $e := %g<a>;
    my &f = sub ($x is rw) { $x = 7 };
    f($e);
    is-deeply %g, %(a => 7), 'is rw through a lexical &f';
}

{
    my %k; my $q := %k<a>;
    (-> $h is raw { my $z := $h<b>; $z = 1 })($q);
    is %k<a><b>, 1, 'a bind to an element below the parameter';
}

{
    my %m;
    my $c2 := (-> $h is raw { $h<a> })(%m);
    my $d := (-> $h is raw { $h<b> })($c2);
    $d = 5;
    is %m<a><b>, 5, 'a returned element bound on through another raw closure';
}

{
    my %p; my $f := %p<a>;
    my $g := (-> \h { h<b> })($f);
    $g = 6;
    is %p<a><b>, 6, 'a sigilless parameter';
}

{
    my %q; my $k := %q<a>;
    (-> $h is raw { $h.defined })($k);
    nok %q<a>:exists, 'passing without a write creates nothing';
    (-> $h { $h })($k);
    nok %q<a>:exists, 'nor does a readonly parameter';
}
