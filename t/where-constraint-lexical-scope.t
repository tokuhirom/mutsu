use v6;
use Test;

plan 23;

# A `where` constraint (and a parameter default) is a closure over the scope
# the SIGNATURE is written in, not over whatever scope happens to be calling.
# mutsu used to evaluate both against the caller's frame, so a routine that
# escaped its declaring scope checked its arguments against the wrong `$a`.

# --- 1. named sub inside a named sub, escaping as the return value ----------
# (roast/S02-types/subset-6e.t #39, roast/6.c/S02-types/subset-6c.t #38)
{
    my $a = 1;                              #OK not used
    {
        my $a = 3;                          #OK not used
        sub producer {
            my $a = 2;
            sub bar($x where $a) { $x }     #OK not used
        }
        my &bar := producer();
        lives-ok { bar(2) }, 'named nested sub: where sees the declaring routine lexical';
        dies-ok  { bar(1) }, 'named nested sub: where rejects a non-matching value';
        dies-ok  { bar(3) }, 'named nested sub: where does not see the enclosing block lexical';
    }
}

# The same escape, but the routine's own body reads the lexical: an escaping
# named sub is lexically scoped, not dynamically scoped.
{
    my $k = 1;                              #OK not used
    {
        my $k = 3;                          #OK not used
        sub kproducer {
            my $k = 2;
            sub kbar() { $k }               #OK not used
        }
        my &kbar := kproducer();
        is kbar(), 2, 'escaping named sub reads its declaring routine lexical';
    }
}

# ...and a parameter DEFAULT, the other declaration-time signature expression.
{
    my $d = 3;                              #OK not used
    sub dproducer {
        my $d = 2;
        sub dbar($x = $d) { $x }            #OK not used
    }
    my &dbar := dproducer();
    is dbar(), 2, 'escaping named sub: parameter default sees the declaring lexical';
}

# --- 2. anonymous sub returned from a routine ------------------------------
{
    my $e = 3;                              #OK not used
    my &ee;
    {
        my $e = 2;
        &ee = sub ($x where $e) { $x };
    }
    lives-ok { ee(2) }, 'escaping anon sub: where sees the declaring block lexical';
    dies-ok  { ee(3) }, 'escaping anon sub: where does not see the outer lexical';
}

{
    sub fproducer {
        my $f = 2;
        return sub ($x where $f) { $x };
    }
    my &ff := fproducer();
    lives-ok { ff(2) }, 'escaping anon sub: where resolves an otherwise-invisible lexical';
    dies-ok  { ff(9) }, 'escaping anon sub: where still rejects';
}

# A `where` closing over the PRODUCING routine's own parameter.
{
    sub gproducer($g) {
        return sub ($x where $g) { $x };
    }
    my &g7 := gproducer(7);
    my &g8 := gproducer(8);
    lives-ok { g7(7) }, 'where over the factory parameter (first instance)';
    dies-ok  { g7(8) }, 'where over the factory parameter rejects the sibling value';
    lives-ok { g8(8) }, 'where over the factory parameter (second instance)';
    dies-ok  { g8(7) }, 'each factory instance keeps its own captured value';
}

# --- 3. pointy block --------------------------------------------------------
{
    my $p = 3;                              #OK not used
    sub pproducer {
        my $p = 2;
        return -> $x where $p { $x };
    }
    my &pp := pproducer();
    lives-ok { pp(2) }, 'escaping pointy block: where sees the declaring lexical';
    dies-ok  { pp(3) }, 'escaping pointy block: where does not see the outer lexical';
}

# --- 4. shadowing at two depths, called in place ----------------------------
{
    my $s = 1;                              #OK not used
    {
        my $s = 3;                          #OK not used
        {
            my $s = 2;
            my &in := sub ($x where $s) { $x };
            lives-ok { in(2) }, 'two-deep shadowing: innermost binding wins';
            dies-ok  { in(3) }, 'two-deep shadowing: middle binding is not used';
        }
    }
}

# --- 5. a `where` on a subset, used from another scope ----------------------
{
    my $limit = 5;
    subset Small of Int where { $_ <= $limit };
    sub takes-small(Small $n) { $n }
    lives-ok { takes-small(3) }, 'subset where closes over its declaring lexical';
    dies-ok  { takes-small(9) }, 'subset where rejects out-of-range values';
}

# --- 6. a `where` on a method parameter ------------------------------------
{
    my $m = 2;
    class WhereMeth {
        method go($x where $m) { $x }
    }
    lives-ok { WhereMeth.new.go(2) }, 'method parameter where sees the enclosing lexical';
    dies-ok  { WhereMeth.new.go(9) }, 'method parameter where rejects';
}

# --- 7. a later parameter constraining on an earlier one -------------------
{
    sub pair-eq($a, $b where $a) { "$a$b" }
    is pair-eq(4, 4), '44', 'where may reference an earlier parameter of the same signature';
    dies-ok { pair-eq(4, 5) }, 'where on an earlier parameter still rejects';
}
