use nqp;

# Shapes the TRIR peephole pass fuses (ADR-0116 §7), each run over enough
# inputs to take both sides of every branch. The routines are native-typed
# so TRIR accepts them; t/vm/codegen/adr0116-trir-peephole.t compares this
# transcript with TRIR on, TRIR off and rakudo's.

# Slot against a constant, all six comparisons, `if` and `unless`.
sub cmp-slot(int $a) {
    my int $r = 0;
    nqp::if(nqp::iseq_i($a, 5), $r = $r + 1);
    nqp::if(nqp::isne_i($a, 5), $r = $r + 2);
    nqp::if(nqp::islt_i($a, -3), $r = $r + 4);
    nqp::if(nqp::isle_i($a, -3), $r = $r + 8);
    nqp::unless(nqp::isgt_i($a, 7), $r = $r + 16);
    nqp::unless(nqp::isge_i($a, 7), $r = $r + 32);
    # A constant too wide for the fused form's operand stays unfused.
    nqp::if(nqp::islt_i($a, 5000000000), $r = $r + 64);
    "$a:$r"
}

# A constant against a computed left operand, and two computed operands.
sub cmp-computed(int $a, int $b) {
    my int $r = 0;
    nqp::if(nqp::iseq_i(nqp::add_i($a, $b), 10), $r = $r + 1);
    nqp::if(nqp::islt_i(nqp::add_i($a, 1), nqp::mul_i($b, 2)), $r = $r + 2);
    my int $x = $a;
    nqp::while(nqp::isgt_i(nqp::sub_i($x, $b), 0), $x = $x - 1);
    "$r/$x"
}

# `||` and `&&` chains as a branch condition (the threaded keep jump), and
# as a value (where the kept operand IS the answer).
sub chains(int $c) {
    my int $r = 0;
    nqp::if($c == 9 || $c == 10, $r = $r + 1);
    nqp::if($c > 0 && $c < 10, $r = $r + 2);
    nqp::if(($c == 1 || $c == 2) && $c != 2, $r = $r + 4);
    my int $v = $c && $c + 100;
    my int $w = $c || 42;
    "$c:$r:$v:$w"
}

# An `nqp::if` in sink position whose arms leave different kinds: one a
# native int, one a num, one a boxed call result. Only the arms' effects
# count.
sub sink-arms(int $c) {
    my int $n = 0;
    my num $f = 0e0;
    nqp::if(nqp::islt_i($c, 2), ($n = $n + 1), ($f = $f + 1e0));
    nqp::if(nqp::iseq_i($c, 0), ($n = $n + 10), nqp::stmts(($f = $f + 2e0), uc("x")));
    "$n/$f"
}

# The `unjsonify-string` loop shape: drain a list, push what survives.
sub drain(str $s) {
    my $codes := nqp::list_i();
    my int $i = 0;
    nqp::while(nqp::islt_i($i, nqp::chars($s)), nqp::stmts(nqp::push_i($codes, nqp::ordat($s, $i)), ($i = $i + 1)));
    my $out := nqp::list_i();
    my int $o;
    nqp::while(nqp::elems($codes),
        nqp::stmts(($o = nqp::shift_i($codes)),
            nqp::if(nqp::iseq_i($o, 45), nqp::null(),
                nqp::if(nqp::iseq_i($o, 9) || nqp::iseq_i($o, 10), nqp::push_i($out, 32), nqp::push_i($out, $o)))));
    my $r := nqp::list_s();
    nqp::while(nqp::elems($out), nqp::push_s($r, nqp::chr(nqp::shift_i($out))));
    nqp::join('', $r)
}

# The fused shift-and-store with a sized native in between: each element
# wraps to 8 bits on the way into the slot, and the loop's back edge is the
# rotated emptiness test.
sub drain-u8(int $n) {
    my $codes := nqp::list_i();
    my int $i = 0;
    nqp::while(nqp::islt_i($i, $n), nqp::stmts(nqp::push_i($codes, nqp::add_i(250, $i)), ($i = $i + 1)));
    my $out := nqp::list_i();
    nqp::while(nqp::elems($codes),
        nqp::stmts((my uint8 $o = nqp::shift_i($codes)), nqp::push_i($out, $o)));
    my $r := nqp::list_s();
    nqp::while(nqp::elems($out), nqp::push_s($r, nqp::shift_i($out)));
    nqp::join(',', $r)
}

say cmp-slot($_) for -4, -3, 0, 5, 6, 7, 8;
say cmp-computed($_, 10 - $_) for 0, 3, 7;
say cmp-computed(9, 2);
say chains($_) for 0, 1, 2, 5, 9, 10, 11;
say sink-arms($_) for 0, 1, 2, 3;
say drain("a-b\tc\nd--e");
say drain("");
say drain-u8($_) for 0, 9;
