use v6;
use Test;

# A user `multi prefix:<++>` / `postfix:<++>` / `prefix:<-->` / `postfix:<-->`
# joins the operator's CANDIDATE SET; it does not replace the operator. Rakudo's
# core set is `Mu:D`, `Mu:U`, `Int:D`, `int`, `uint`, `Bool`, `Num:D`, `Num:U`,
# `num`, so a user candidate only wins for the types the core set covers with
# `Mu:D`/`Mu:U` alone -- an untyped user parameter is `Any`, narrower than `Mu`
# but wider than `Int:D`/`Bool`/`Num:D`.
#
# Every expectation below was measured against rakudo. Each row runs in its own
# EVAL so the candidate sets of different rows cannot leak into each other.

plan 47;

my $USER = 'multi prefix:<++>($a) is default { $a - 1 }; ';
my $USERD = 'multi prefix:<-->($a) is default { $a + 10 }; ';
my $POST = 'multi postfix:<++>($a) is default { "USER" }; ';
my $POSTD = 'multi postfix:<-->($a) is default { "USERD" }; ';

# --- prefix ++ : the core typed candidates win -------------------------------
is EVAL($USER ~ 'my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'prefix ++ on Int runs the core Int:D candidate, and mutates';
is EVAL($USER ~ 'my $b = True; my $v = ++$b; "$v/$b"'), 'True/True',
    'prefix ++ on Bool runs the core Bool candidate';
is EVAL($USER ~ 'my $b = False; my $v = ++$b; "$v/$b"'), 'True/True',
    'prefix ++ on False runs the core Bool candidate';
is EVAL($USER ~ 'my $n = 1e0; my $v = ++$n; "$v/$n"'), '2/2',
    'prefix ++ on Num runs the core Num:D candidate';
is EVAL($USER ~ 'my $x = <42>; my $v = ++$x; "$v/$x"'), '43/43',
    'prefix ++ on an allomorph runs the core Int:D candidate';

# --- prefix ++ : everything else reaches the user candidate ------------------
is EVAL($USER ~ 'my $r = 1/2; my $v = ++$r; "$v/$r"'), '-0.5/0.5',
    'prefix ++ on Rat runs the user candidate, and does NOT mutate';
is EVAL($USER ~ 'my $u; my $v = ++$u; "$v/{$u.raku}"'), '-1/Any',
    'prefix ++ on an undefined Any runs the user candidate';
is EVAL($USER ~ 'my Str $s; my $v = ++$s; "$v/{$s.raku}"'), '-1/Str',
    'prefix ++ on a Str type object runs the user candidate';
is EVAL($USER ~ 'my @a; my $v = ++@a[0]; "$v/{@a[0].raku}"'), '-1/Any',
    'prefix ++ on an undefined array element runs the user candidate';
is EVAL($USER ~ 'my %h; my $v = ++%h<k>; "$v/{%h<k>.raku}"'), '-1/Any',
    'prefix ++ on an undefined hash element runs the user candidate';

# --- definiteness: the core set has Int:D but no Int:U -----------------------
is EVAL($USER ~ 'my Num $n; my $v = ++$n; "$v/$n"'), '1/1',
    'prefix ++ on a Num type object runs the core Num:U candidate';
is EVAL($USER ~ 'my Bool $b; my $v = ++$b; "$v/$b"'), 'True/True',
    'prefix ++ on a Bool type object runs the core Bool candidate';

# --- prefix -- mirrors prefix ++ --------------------------------------------
is EVAL($USERD ~ 'my $i = 1; my $v = --$i; "$v/$i"'), '0/0',
    'prefix -- on Int runs the core Int:D candidate';
is EVAL($USERD ~ 'my $b = True; my $v = --$b; "$v/$b"'), 'False/False',
    'prefix -- on Bool runs the core Bool candidate';
is EVAL($USERD ~ 'my $n = 1e0; my $v = --$n; "$v/$n"'), '0/0',
    'prefix -- on Num runs the core Num:D candidate';
is EVAL($USERD ~ 'my $r = 1/2; my $v = --$r; "$v/$r"'), '10.5/0.5',
    'prefix -- on Rat runs the user candidate';
is EVAL($USERD ~ 'my $u; my $v = --$u; "$v/{$u.raku}"'), '10/Any',
    'prefix -- on an undefined Any runs the user candidate';

# --- postfix ++ / -- consult the user candidate too --------------------------
is EVAL($POST ~ 'my $i = 1; my $v = $i++; "$v/$i"'), '1/2',
    'postfix ++ on Int runs the core Int:D candidate';
is EVAL($POST ~ 'my $n = 1e0; my $v = $n++; "$v/$n"'), '1/2',
    'postfix ++ on Num runs the core Num:D candidate';
is EVAL($POST ~ 'my $b = True; my $v = $b++; "$v/$b"'), 'True/True',
    'postfix ++ on Bool runs the core Bool:D candidate';
is EVAL($POST ~ 'my $s = "abc"; my $v = $s++; "$v/$s"'), 'USER/abc',
    'postfix ++ on Str runs the user candidate, and does NOT magic-increment';
is EVAL($POST ~ 'my $r = 1/2; my $v = $r++; "$v/$r"'), 'USER/0.5',
    'postfix ++ on Rat runs the user candidate';
is EVAL($POST ~ 'my $u; my $v = $u++; "$v/{$u.raku}"'), 'USER/Any',
    'postfix ++ on an undefined Any runs the user candidate';
is EVAL($POST ~ 'my Int $i; my $v = $i++; "$v/{$i.raku}"'), 'USER/Int',
    'postfix ++ on an Int type object runs the user candidate (no core Int:U)';
is EVAL($POST ~ 'my Num $n; my $v = $n++; "$v/$n"'), '0/1',
    'postfix ++ on a Num type object runs the core Num:U candidate';
is EVAL($POST ~ 'my Bool $b; my $v = $b++; "$v/$b"'), 'False/True',
    'postfix ++ on a Bool type object runs the core Bool:U candidate';
is EVAL($POSTD ~ 'my $i = 1; my $v = $i--; "$v/$i"'), '1/0',
    'postfix -- on Int runs the core Int:D candidate';
is EVAL($POSTD ~ 'my $s = "abc"; my $v = $s--; "$v/$s"'), 'USERD/abc',
    'postfix -- on Str runs the user candidate';

# --- a typed user candidate --------------------------------------------------
is EVAL('multi prefix:<++>(Str $a) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'a non-matching user candidate leaves the core increment mutating';
is EVAL('multi prefix:<++>(Str $a) { "USER" }; my $s = "abc"; my $v = ++$s; "$v/$s"'), 'USER/abc',
    'a matching Str user candidate wins for a Str argument';
is EVAL('multi prefix:<++>(Int $a) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'a user Int candidate loses to the narrower core Int:D';
is EVAL('multi prefix:<++>(Int:D $a) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'an equally narrow user Int:D candidate loses the tie to core';
is EVAL('multi prefix:<++>(Cool $a) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'a wider user Cool candidate loses to the core Int:D';
is EVAL('multi prefix:<++>(Any $a) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'an explicit Any user candidate loses to the core Int:D';
is EVAL('multi prefix:<++>(Mu $a) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'an explicit Mu user candidate loses to the core Int:D';
is EVAL('multi prefix:<++>(Mu $a) { $a + 1 }; my $r = 1/2; my $v = ++$r; "$v/$r"'), '1.5/1.5',
    'a user Mu candidate ties with the core Mu:D, and the tie goes to core';
is EVAL('multi prefix:<++>(Any $a) { "USER" }; my $r = 1/2; my $v = ++$r; "$v/$r"'), 'USER/0.5',
    'a user Any candidate out-narrows the core Mu:D';
is EVAL('multi prefix:<++>(Rat $a) { "USER" }; my $r = 1/2; my $v = ++$r; "$v/$r"'), 'USER/0.5',
    'a user Rat candidate wins for a Rat argument';
is EVAL('multi prefix:<++>($a where * > 0) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'a where-constrained but untyped user candidate still loses to the core Int:D';
is EVAL('subset MyHalf of Rat; multi prefix:<++>(MyHalf $a) { "USER" }; my $r = 1/2; my $v = ++$r; "$v/$r"'),
    'USER/0.5', 'a subset user candidate out-narrows the core Mu:D';

# --- `is rw` on the user candidate does not change the ranking ---------------
my $RW = 'multi prefix:<++>($a is rw) is default { $a = 99; "RW" }; ';
is EVAL($RW ~ 'my $i = 1; my $v = ++$i; "$v/$i"'), '2/2',
    'an is-rw user candidate still loses to the core Int:D';
is EVAL($RW ~ 'my $s = "abc"; my $v = ++$s; "$v/$s"'), 'RW/99',
    'an is-rw user candidate wins for a Str argument and writes back';
is EVAL($RW ~ 'my $r = 1/2; my $v = ++$r; "$v/$r"'), 'RW/99',
    'an is-rw user candidate wins for a Rat argument and writes back';

# --- a plain `sub` is a lexical shadow, not a candidate ----------------------
is EVAL('sub prefix:<++>($a) { "USER" }; my $i = 1; my $v = ++$i; "$v/$i"'), 'USER/1',
    'a non-multi sub prefix:<++> replaces the operator outright';
is EVAL('sub prefix:<++>($a) { "USER" }; my $r = 1/2; my $v = ++$r; "$v/$r"'), 'USER/0.5',
    'a non-multi sub prefix:<++> replaces the operator for every type';

# --- with no user candidate at all, nothing changes --------------------------
is EVAL('my $s = "abc"; my $v = ++$s; "$v/$s"'), 'abd/abd',
    'without a user candidate the magic string increment is untouched';
is EVAL('my $s = "abc"; my $v = $s++; "$v/$s"'), 'abc/abd',
    'without a user candidate postfix ++ is untouched';

done-testing;
