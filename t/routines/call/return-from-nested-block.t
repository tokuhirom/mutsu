use Test;

# A `return` inside a block that was created while another block was running
# leaves the enclosing routine. Every block invocation rebinds its own callable
# id, so the inner block used to capture the outer *block* as its return target
# and escaped the routine as "Attempt to return outside of
# immediately-enclosing Routine" (#9630).

plan 6;

sub two-levels {
    my $b = { my $c = -> { return 5 }; $c() };
    $b();
    6
}
is two-levels(), 5, 'return from a block nested in a called block';

sub three-levels {
    my $a = { my $b = { my $c = { return 'deep' }; $c() }; $b() };
    $a();
    'fell through'
}
is three-levels(), 'deep', 'return through three levels of called blocks';

sub inner-routine {
    my $b = {
        my sub g { my $c = { return 'g' }; $c(); 'g fell through' }
        g() ~ '+block'
    };
    $b()
}
is inner-routine(), 'g+block', 'a routine declared in a block still owns its return';

sub pointy-arg {
    my &outer = -> $x { my &inner = -> $y { return $x + $y }; inner(10) };
    outer(1);
    0
}
is pointy-arg(), 11, 'pointy blocks with arguments return from the routine';

sub via-map {
    my $b = { (1, 2, 3).map({ return $_ if $_ == 2 }).eager };
    $b();
    0
}
is via-map(), 2, 'return from a map block inside a called block';

sub once-per-clone {
    my @out;
    for 1..3 { my $c = { once @out.push: 'x' }; $c() }
    @out.elems
}
is once-per-clone(), 3, 'once still fires per block clone';
