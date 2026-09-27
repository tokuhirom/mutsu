use Test;

# A `method` declarator is has-scoped: wherever it lexically sits inside a
# package body -- a bare block, a `do { }`, an `if` branch -- it installs the
# method in that package, while its body closes over the block's lexicals
# (#9525).

plan 21;

{
    role I { method unrecord { ... } }
    class T does I {
        {
            sub helper($x) { $x * 2 }
            my $y = 1;
            method unrecord { helper(21) + $y }
        }
    }
    is T.new.unrecord, 43, 'a method in a bare block satisfies a role requirement';
}

{
    class U { { method m { 3 } }; method n { 1 } }
    is-deeply U.^methods(:local)».name.grep(* ne 'POPULATE').sort.List, <m n>, 'the method is in the method table';
    is U.can('m').elems, 1, '.can finds it';
}

{
    class V { if False { method z { 5 } } }
    is V.new.z, 5, 'the method is installed even when its block never runs';
}

{
    class W {
        do {
            sub h { 9 }
            method !p { h() }
            submethod s { 1 }
            method r(--> Int) { self!p }
        }
    }
    is W.new.r, 9, 'private method and return type from a do block';
    is-deeply W.^methods(:local)».name.grep(* ne 'POPULATE').sort.List, <r s>, 'submethod is installed too';
    is W.^lookup('r').returns.^name, 'Int', 'the return type is kept';
}

{
    role Inst { method unrecord { ... } }
    class Tuple does Inst {
        has @!record = 1, 2, 3;
        do { # hide this sub (Data::Record's shape)
            proto sub unrecord(Mu) is raw          {*}
            multi sub unrecord(Inst:D \recorded) { recorded.unrecord }
            multi sub unrecord(Mu \value)          { value }
            method unrecord(::?CLASS:D: --> List:D) {
                @!record.map(&unrecord).List
            }
        }
    }
    is-deeply Tuple.new.unrecord, (1, 2, 3), 'a same-named proto/multi sub in the do block is the one &name reads';
    is Tuple.^lookup('unrecord').returns.^name, 'List:D', 'its return type is kept';
}

{
    class T9 { { sub unr($v) { $v * 2 }; method b { &unr } } }
    is T9.new.b.(4), 8, '&name of a bare block sub';
    class T0 { do { sub unr($v) { $v * 3 }; method a { unr(3) }; method b { &unr } } }
    is T0.new.a, 9, 'bare call of a do block sub';
    is T0.new.b.(2), 6, '&name of a do block sub';
}

{
    class K { my $count = 0; { my $step = 2; method inc { $count += $step } }; method get { $count } }
    my $k = K.new;
    $k.inc; $k.inc;
    is $k.get, 4, 'a class-body lexical written through a nested-block method';
    class M { { my $c = 0; method bump { ++$c } } }
    my $m = M.new;
    $m.bump;
    is $m.bump, 2, "the block's own lexical keeps its state across calls";
}

{
    class Sig { { my $def = 10; method s($x = $def) { $x } } }
    is Sig.new.s, 10, 'a parameter default reads the block lexical';
    class Mu2 { do { multi method mm(Int $x) { "int $x" }; multi method mm(Str $x) { "str $x" } } }
    is Mu2.new.mm("a"), 'str a', 'multi methods from a do block';
}

{
    role R[$n] { do { sub h { $n * 10 }; my $q = $n; method rm { h() + $q } } }
    class A does R[1] {}
    class B does R[2] {}
    is A.new.rm, 11, 'a role body block closes over its parameter (first composition)';
    is B.new.rm, 22, 'each composition gets its own capture';
    role Q[$n] { do { sub h($x) { $x * $n }; method qm { (1, 2).map(&h).List } } }
    class QA does Q[3] {}
    is-deeply QA.new.qm, (3, 6), '&name of a role body do-block sub';
    role Mx { do { my $v = 9; method mv { $v } } }
    is (1 but Mx).mv, 9, 'a mixin sees the capture';
}

{
    my @got;
    for 1..2 -> $i { my class Lp { { my $c = $i; method c { $c } } }; @got.push: Lp.new.c }
    is-deeply @got, [1, 2], 'a class declared in a loop captures each iteration';
}
