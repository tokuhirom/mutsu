use Test;
use nqp;

# The Rakudo-only `p6*` nqp ops (#11505). Each expectation matches rakudo
# 2026.09 except where noted: `p6decontrv` and `p6typecheckrv` take their
# code object as a compile-time constant in rakudo (a `QAST::WVal` only the
# compiler itself produces), so their expected values come from what the
# Rakudo binder computes for that code object; and rakudo emits `p6return`
# only inside the RETURN handler it wraps around a routine body, where it is
# `return`, which is what it means here anywhere in a routine.

plan 50;

# -- value ops --------------------------------------------------------------

{
    my $u;
    is nqp::p6definite(42), True, 'p6definite: a concrete value';
    is nqp::p6definite(Int), False, 'p6definite: a type object';
    is nqp::p6definite($u), False, 'p6definite: an unset variable';
    isa-ok nqp::p6definite(1), Bool, 'p6definite answers a Bool';
}

{
    is nqp::p6box(nqp::unbox_s("x")).^name, 'Str', 'p6box: a native str';
    is nqp::p6box(nqp::unbox_i(3)).^name, 'Int', 'p6box: a native int';
    is nqp::p6box(nqp::unbox_n(1e0)).^name, 'Num', 'p6box: a native num';
    my int $i = 5;
    is nqp::p6box($i), 5, 'p6box: a native variable';
    is nqp::p6box([1]).^name, 'Array', 'p6box: an object is itself';
}

{
    my $x = 1;
    nqp::p6store($x, 5);
    is $x, 5, 'p6store assigns a scalar';
    is nqp::p6store($x, 7), 7, 'p6store yields the stored value';
    my @a;
    nqp::p6store(@a, (1, 2));
    is-deeply @a, [1, 2], 'p6store stores into an array';
    my %h;
    nqp::p6store(%h<a>, 3);
    is-deeply %h, %(a => 3), 'p6store assigns an element';
    my Int $t;
    throws-like { nqp::p6store($t, "s") }, X::TypeCheck::Assignment,
        'p6store type-checks like assignment';
}

{
    my @sunk;
    my class S { has $.n; method sink { @sunk.push($!n) } }
    nqp::p6sink(S.new(n => 1));
    my $s = S.new(n => 2);
    nqp::p6sink($s);
    is-deeply @sunk, [1], 'p6sink sinks a fresh value but not a container';
    is nqp::p6sink(5), 5, 'p6sink yields its operand';
    throws-like { nqp::p6sink(fail("boom")); 1 }, X::AdHoc,
        'p6sink of an unhandled Failure throws';
}

{
    sub plain { 1 }
    sub lvalue is rw { 1 }
    my $x = 1;
    is nqp::p6decontrv(&plain, $x), 1, 'p6decontrv: the value of a plain routine';
    is nqp::p6decontrv_6c(&lvalue, $x), 1, 'p6decontrv_6c: an rw routine';
    throws-like { nqp::p6decontrv(Int, 1) }, Exception,
        message => /'rw'/, 'p6decontrv needs a code object';
}

{
    sub typed(--> Int) { 1 }
    sub untyped { 1 }
    is nqp::p6typecheckrv(1, &typed), 1, 'p6typecheckrv: a matching value';
    is nqp::p6typecheckrv(Nil, &typed), Nil, 'p6typecheckrv: Nil always passes';
    is nqp::p6typecheckrv("a", &untyped), "a", 'p6typecheckrv: no return type';
    throws-like { nqp::p6typecheckrv("a", &typed) }, X::TypeCheck::Return,
        'p6typecheckrv rejects a mismatch';
}

# -- binder ops -------------------------------------------------------------

{
    is nqp::p6bindassert(1, Int), 1, 'p6bindassert passes a matching value';
    throws-like { nqp::p6bindassert("a", Int) }, X::TypeCheck::Binding,
        message => 'Type check failed in binding; expected Int but got Str ("a")',
        'p6bindassert rejects a mismatch';
}

sub two(Int $x, $y) { }

{
    is nqp::p6isbindable(&two.signature, \(1, 2)), 1, 'p6isbindable: binds';
    is nqp::p6isbindable(&two.signature, \("a", 2)), 0, 'p6isbindable: wrong type';
    is nqp::p6isbindable(&two.signature, \(1)), 0, 'p6isbindable: too few';
    is nqp::p6isbindable(&two.signature, \(1, 2, 3)), 0, 'p6isbindable: too many';
    my $x = 'outer';
    nqp::p6isbindable(&two.signature, \(1, 2));
    is $x, 'outer', 'p6isbindable binds nothing in the caller';
}

{
    # The binder binds into the caller's lexicals, which must exist.
    sub caller-of { my ($x, $y); nqp::p6bindcaptosig(&two.signature, \(1, 2)) }
    is caller-of().raku, ':(Int $x, $y)', 'p6bindcaptosig returns the signature';
    throws-like { my ($x, $y); nqp::p6bindcaptosig(&two.signature, \("a", 2)) },
        X::TypeCheck::Binding::Parameter,
        message => /'parameter \'$x\'; expected Int but got Str ("a")'/,
        'p6bindcaptosig raises the binder error';
}

{
    my class A { }
    my class B is A { }
    sub b(B $b) { }
    sub n(int $i) { }
    sub c(|c) { }
    sub o($a, $b?) { }
    sub d(Int:D $a) { }
    is nqp::p6trialbind(&two.signature, nqp::list(1, 2), nqp::list(0, 0)), 1,
        'p6trialbind: always binds';
    is nqp::p6trialbind(&two.signature, nqp::list(Any, 2), nqp::list(0, 0)), 0,
        'p6trialbind: a supertype argument is not sure';
    is nqp::p6trialbind(&b.signature, nqp::list(A.new), nqp::list(0)), 0,
        'p6trialbind: a parent class argument is not sure';
    is nqp::p6trialbind(&b.signature, nqp::list(1), nqp::list(0)), -1,
        'p6trialbind: an unrelated type never binds';
    is nqp::p6trialbind(&two.signature, nqp::list("x", 2), nqp::list(3, 0)), -1,
        'p6trialbind: a native str for an Int never binds';
    is nqp::p6trialbind(&n.signature, nqp::list(1), nqp::list(3)), -1,
        'p6trialbind: the wrong native never binds';
    is nqp::p6trialbind(&n.signature, nqp::list(1), nqp::list(0)), 0,
        'p6trialbind: an object for a native is not sure';
    is nqp::p6trialbind(&c.signature, nqp::list(1), nqp::list(0)), 1,
        'p6trialbind: a lone capture takes anything';
    is nqp::p6trialbind(&two.signature, nqp::list(1), nqp::list(0)), -1,
        'p6trialbind: too few';
    is nqp::p6trialbind(&o.signature, nqp::list(1), nqp::list(0)), 1,
        'p6trialbind: an optional parameter may be left out';
    is nqp::p6trialbind(&d.signature, nqp::list(1), nqp::list(0)), 0,
        'p6trialbind: a definedness smiley is decided at run time';
}

# -- code and control ops ---------------------------------------------------

{
    is nqp::p6capturelex(42), 42, 'p6capturelex returns a non-code operand';
    is nqp::p6capturelex(sub { 7 })(), 7, 'p6capturelex returns the closure';

    my $v = 'captured';
    my $code = sub { $v };
    my $ctx := nqp::p6getouterctx($code);
    is nqp::atkey(nqp::ctxlexpad($ctx), '$v'), 'captured',
        'p6getouterctx: the closure scope';

    my &auto = -> |c { 42 };
    ok nqp::p6setautothreader(&auto) === &auto, 'p6setautothreader returns the callable';
}

{
    sub early { nqp::p6return(5); 7 }
    is early(), 5, 'p6return returns from the routine';

    sub count(*@a) { @a.elems }
    is nqp::p6invokeflat(&count, nqp::list(1, 2, (3, 4))), 4,
        'p6invokeflat flattens the argument list';
}
