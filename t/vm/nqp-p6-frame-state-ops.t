use Test;
use nqp;

# The Rakudo `p6*` nqp ops that read the running frame (#11505, ADR-11505).
# Every expectation matches rakudo 2026.09.

plan 39;

# -- p6argvmarray: the call's positional arguments ----------------------------

{
    sub two($a, $b) { nqp::p6argvmarray() }
    my $l := two(1, 'x');
    is nqp::elems($l), 2, 'p6argvmarray: one element per positional';
    is nqp::atpos($l, 0), 1, 'p6argvmarray: first argument';
    is nqp::atpos($l, 1), 'x', 'p6argvmarray: second argument';
    is $l.^name, 'BOOTArray', 'p6argvmarray: a raw list';
}

{
    sub anon(|) { nqp::p6argvmarray() }
    my $l := anon(1, [2, 3], :x(4));
    is nqp::elems($l), 2, 'p6argvmarray: named arguments are left out';
    is nqp::atpos($l, 1), [2, 3], 'p6argvmarray: an argument is not flattened';
    is nqp::elems(anon(|(1, 2, 3), |%(a => 1))), 3, 'p6argvmarray: slips arrive flattened';
    sub cap($a, $b) { nqp::usecapture() }
    is nqp::captureposelems(cap(1, 2)), 2, 'usecapture: a one-expression routine records its capture';
    is nqp::atpos(anon((a => 1)), 0).^name, 'Pair', 'p6argvmarray: a positional Pair stays positional';
}

{
    sub opt($a, $b?) { nqp::elems(nqp::p6argvmarray()) }
    is opt(1), 1, 'p6argvmarray: a defaulted parameter adds no argument';
    is opt(1, 2), 2, 'p6argvmarray: an optional argument that was passed';
}

{
    my class A {
        method count($x) { nqp::elems(nqp::p6argvmarray()) }
        method first($x) { nqp::atpos(nqp::p6argvmarray(), 0).^name }
        method second($x) { nqp::atpos(nqp::p6argvmarray(), 1) }
    }
    is A.count(5), 2, 'p6argvmarray: a method counts its invocant';
    is A.first(5), 'A', 'p6argvmarray: the invocant comes first';
    my $o = A.new;
    is (^4).map({ $o.second($_) }).join(','), '0,1,2,3',
        'p6argvmarray: each repeated method call sees its own arguments';
    my &c = -> $p, $q { nqp::elems(nqp::p6argvmarray()) };
    is c(1, 2), 2, 'p6argvmarray: a pointy block';
}

# -- p6bindsig / p6trybindsig: the frame is already bound ---------------------

{
    sub rebind($a is copy) {
        $a = 5;
        nqp::p6bindsig();
        my $after-bind = $a;
        my $try = nqp::p6trybindsig();
        ($after-bind, $try, $a)
    }
    my ($after-bind, $try, $a) = rebind(1);
    is $after-bind, 5, 'p6bindsig: a bound frame is not bound again';
    is $try, 1, 'p6trybindsig: a bound frame answers 1';
    is $a, 5, 'p6trybindsig: a bound frame is not bound again';
    sub null-bind($a) { nqp::isnull(nqp::p6bindsig()) }
    ok null-bind(1), 'p6bindsig answers null';
}

# -- p6setpre / p6clearpre ----------------------------------------------------

{
    sub pre { (nqp::isnull(nqp::p6setpre()), nqp::isnull(nqp::p6clearpre())) }
    is-deeply pre(), (1, 1), 'p6setpre / p6clearpre answer null';
}

# -- p6stateinit: the first run of a frame with state variables ---------------

{
    sub st { state $x; nqp::p6stateinit() }
    is (st(), st()).join, '10', 'p6stateinit: 1 on the first run only';
    sub before { my $r = nqp::p6stateinit(); state $x; $r }
    is (before(), before()).join, '10', 'p6stateinit: the flag is the whole frame\'s';
    sub anon-state { $++; nqp::p6stateinit() }
    is (anon-state(), anon-state()).join, '10', 'p6stateinit: an anonymous state variable counts';
    sub none { nqp::p6stateinit() }
    is (none(), none()).join, '00', 'p6stateinit: 0 in a frame without state';
    sub inner { if 1 { state $x }; nqp::p6stateinit() }
    is (inner(), inner()).join, '00', 'p6stateinit: an inner block\'s state is its own';
    sub inlined { state $x; if 1 { nqp::p6stateinit() } }
    is (inlined(), inlined()).join, '10', 'p6stateinit: an inlined block reads its frame';
    sub loop { state $x; my $s = ''; for 1..2 { $s ~= nqp::p6stateinit() }; $s }
    is (loop(), loop()).join(' '), '11 00', 'p6stateinit: a statement-level loop body is inlined';
    sub closure { state $x; my $s = ''; (1, 2).map({ $s ~= nqp::p6stateinit() }).eager; $s }
    is closure(), '00', 'p6stateinit: a closure is a frame of its own';
}

{
    my @c = (^2).map({ -> { state $x; nqp::p6stateinit() } });
    is (@c[0](), @c[0](), @c[1]()).join, '101', 'p6stateinit: per closure clone';
    my $s = '';
    for 1..2 { state $x; $s ~= nqp::p6stateinit() }
    is $s, '10', 'p6stateinit: a loop body with state is a frame';
}

# -- p6setfirstflag / p6takefirstflag: a code object's FIRST flag -------------

{
    sub never { nqp::p6takefirstflag() }
    is (never(), never()).join, '00', 'p6takefirstflag: 0 when nothing set the flag';
    sub self-set {
        nqp::p6setfirstflag(nqp::getattr(&self-set, Code, '$!do'));
        nqp::p6takefirstflag() ~ nqp::p6takefirstflag()
    }
    is (self-set(), self-set()).join(' '), '10 10', 'p6takefirstflag: reads and clears the flag';
    sub later { nqp::p6takefirstflag() }
    nqp::p6setfirstflag(nqp::getattr(&later, Code, '$!do'));
    is (later(), later()).join, '10', 'p6setfirstflag: set from outside the routine';
    sub inlined { if 1 { nqp::p6takefirstflag() } }
    nqp::p6setfirstflag(nqp::getattr(&inlined, Code, '$!do'));
    is (inlined(), inlined()).join, '10', 'p6takefirstflag: an inlined block reads its routine';
    sub outer-only { my $c = { nqp::p6takefirstflag() }; $c() }
    nqp::p6setfirstflag(nqp::getattr(&outer-only, Code, '$!do'));
    is outer-only(), 0, 'p6takefirstflag: a closure reads its own flag';
    is nqp::p6takefirstflag(), 0, 'p6takefirstflag: 0 in the mainline';
}

{
    my &b = { nqp::p6takefirstflag() };
    nqp::p6setfirstflag(nqp::getattr(nqp::decont(&b), Code, '$!do'));
    is (b(), b()).join, '10', 'p6setfirstflag: a block';
    my @c = (^2).map({ -> { nqp::p6takefirstflag() } });
    nqp::p6setfirstflag(nqp::getattr(nqp::decont(@c[1]), Code, '$!do'));
    is (@c[0](), @c[1](), @c[1]()).join, '010', 'p6setfirstflag: the flag is per closure clone';
    sub plain { 1 }
    throws-like { nqp::p6setfirstflag(&plain) }, Exception,
        message => 'p6setfirstflag requires a bytecode handle',
        'p6setfirstflag: a Code object is not a bytecode handle';
}
