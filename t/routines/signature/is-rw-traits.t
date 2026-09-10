use Test;

plan 5;

{
    sub assign-rw($a is rw) {
        $a = 42;
    }

    my $x = 1;
    assign-rw($x);
    is $x, 42, 'rw parameter writes back to caller variable';
    dies-ok { assign-rw(7) }, 'rw parameter rejects non-variable argument';
}

{
    my $bump = -> $v is rw { $v++ };
    my $n = 10;
    $bump($n);
    is $n, 11, 'rw trait is preserved for pointy-block parameters';
}

{
    my %h = (a => 1);
    %h.pairs[0].value = 3;
    for %h.pairs -> $pair {
        $pair.value += 5;
    }
    is %h<a>, 8, 'pair.value assignment and compound assignment update hash entries';
}

throws-like { Q/sub foo(+$x [$ is rw = False]) { $x }/.EVAL }, X::Trait::Invalid,
    'invalid rw trait placement throws X::Trait::Invalid';
