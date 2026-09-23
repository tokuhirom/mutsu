use nqp;

# Fixture for t/vm/codegen/adr0112-trir-string-path.t: one routine per
# construct ADR-0112 Step 2 admits into TRIR, each called twice (a TRIR bug
# gives a right first answer and a wrong later one).

# `:=`-bound nqp result in native arithmetic.
sub after-quote(str $t, int $from) {
    my $end := nqp::index($t, '"', $from);
    nqp::sub_i($end + 1, $from)
}

# A sigilless parameter under a nominal `:D` check, mutated in place.
sub drain(Uni:D \codes, int $keep) {
    my int $sum = 0;
    nqp::while(
      nqp::isgt_i(nqp::elems(codes), $keep),
      ($sum = nqp::add_i($sum, nqp::shift_i(codes)))
    );
    $sum
}

# Sized native integers wrap on every store.
sub sized(int $n) {
    my uint32 $u = nqp::sub_i(0, $n);
    my int8 $s = nqp::add_i(100, $n);
    $s = nqp::add_i($s, 100);
    "$u $s"
}

# A call-only inner sub, inlined: it reads the enclosing routine's
# sigilless and native parameters, declares its own `my`, shadows a name the
# routine also declares, and is called twice.
sub hex4(Uni:D \codes, int $at) {
    my int $v = 0;
    my sub fetch() {
        my int $v = 0;
        my int $times = 5;
        nqp::while(
          ($times = nqp::sub_i($times, 1)),
          nqp::stmts(
            (my uint32 $o = nqp::shift_i(codes)),
            ($v = nqp::add_i(nqp::mul_i($v, 16),
              nqp::if(nqp::isge_i($o, 97), nqp::sub_i($o, 87), nqp::sub_i($o, 48))))
          )
        );
        nqp::add_i($v, $at)
    }
    my int $first = fetch;
    my int $second = fetch();
    "$first $second $v"
}

# Method callouts on a computed value, a native, and a bound value.
sub numeric(str $t, int $from, int $len) {
    my $r := nqp::substr($t, $from, $len).Numeric;
    nqp::if(
      nqp::istype($r, Failure),
      nqp::stmts($r.Bool, "bad {nqp::substr($t, $from, $len).raku}"),
      "$r {$len.base(16)}"
    )
}

# Definite return values.
sub yes(int $p is rw --> True)  { $p = $p + 4 }
sub no(int $p is rw --> False)  { $p = $p + 5 }

# A native store of an nqp hole dies as the assignment does.
sub hole(int $i) {
    my $l := nqp::list();
    nqp::bindpos($l, 3, 7);
    my int $x = nqp::atpos($l, $i);
    $x
}

for ^2 {
    say "after-quote => ", after-quote('ab"cd"', 1), " ", after-quote('"x"', 1);
    say "drain => ", drain(nqp::strtocodes('abc', nqp::const::NORMALIZE_NFD, nqp::create(NFD)), 1);
    say "sized => ", sized(1), " / ", sized(28);
    say "hex4 => ", hex4(nqp::strtocodes('00410fff', nqp::const::NORMALIZE_NFD, nqp::create(NFD)), 2);
    say "numeric => ", numeric("x12.5e1y", 1, 6), " | ", numeric("abc", 0, 3);
    my int $p = 1;
    my $y = yes($p);
    my $a = $p;
    my $n = no($p);
    say "definite => $y $a $n $p";
    say "hole => ", hole(3), " ", (try hole(1)) // $!.message;
    say "type check => ", (try drain(Uni, 0)) // $!.^name;
}
