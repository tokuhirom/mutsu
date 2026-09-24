# Pins the by-name readers and writers that used to be covered by a
# whole-frame locals <-> env synchronisation around individual opcodes, which
# #9169 removed (each cost O(frame locals) per execution): `say`/`print`/
# `put`/`note`, `~~`, the reverse sigilless-alias propagation of `SetGlobal`,
# `but`/`does`, a user `.defined` behind `andthen`/`orelse`, a user `sink`,
# and `eager` over a `gather`. Each case reads or writes an outer lexical
# from the code the op runs, after that lexical changed, and checks that
# both sides agree.
use Test;

plan 25;

# --- say / put / print: a user .gist / $*OUT override reads fresh lexicals ---
{
    my $count = 0;
    my $name  = "a";
    my class G { method gist { "G($name,$count)" } }
    $name  = "b";
    $count = 5;
    my $out = '';
    my class Cap {
        method print(*@a) { $out ~= @a.join; True };
        method say(*@a)   { $out ~= @a.join ~ "\n"; True };
        method put(*@a)   { $out ~= @a.join ~ "\n"; True }
    }
    my $tag = 'x';
    my class Tagged { method Str { "T$tag" }; method gist { "G$tag" } }
    {
        my $*OUT = Cap.new;
        say G.new;
        $count++;
        say G.new;
        $tag = 'y';
        put Tagged.new;
        print Tagged.new;
    }
    is $out, "G(b,5)\nG(b,6)\nTy\nTy", 'say/put/print see the current value of outer lexicals';
}

# --- ~~ against a regex literal interpolating a lexical ---
{
    my $y = "b";
    my $x = "abc";
    ok  $x ~~ /a$y/, 'regex literal interpolates the current value';
    $y = "z";
    nok $x ~~ /a$y/, 'regex literal sees a later reassignment';
    $y = "c";
    ok  $x ~~ m/ b $y /, 'm// interpolates the current value';
    my @alts = <q c>;
    ok  $x ~~ / b @alts /, 'array interpolation';
    my $dash-name = "c";
    ok  $x ~~ /b$dash-name/, 'kebab-case interpolation';
    $_ = "hello";
    my $w = "e";
    ok  so(/h$w/), 'bare regex against the topic interpolates';
}

# --- ~~ against a regex *value* (computed RHS) ---
{
    my $y = "b";
    my $re = /a$y/;
    ok  "ab" ~~ $re, 'stored regex matches';
    my $z = "q";
    ok  "aq" ~~ (/a$z/ | /zzz/), 'junction of regexes interpolates';
    my $lo = 3;
    ok  5 ~~ $lo..10, 'computed range RHS';
    my $t = "x";
    my $str = "xyz";
    $str ~~ s/$t/Q/;
    is  $str, "Qyz", 's/// interpolates the current value';
    my $cnt = 0;
    "aaa" ~~ / [ a { $cnt++ } ]+ /;
    is  $cnt, 3, 'embedded regex code writes an outer lexical';
    my $k = "z";
    my $hit = '';
    given "abz" { when /b$k/ { $hit = 'when' } }
    is  $hit, 'when', 'when-regex interpolates the current value';
}

# --- ~~ against a type / constant keeps working and writes $/ back ---
{
    my $n = 5;
    ok  $n ~~ Int, 'type RHS';
    ok  $n ~~ 5,   'constant RHS';
    if "hello" ~~ /l(l)/ { is ~$0, "l", 'numbered capture written back' }
}

# --- SetGlobal: a store reaches the variables aliased to its target ---
{
    $_ = 1;
    my $c := $_;
    $_ = 5;
    is $c, 5, 'a store to $_ reaches `my $c := $_`';
    sub f(\x) { x = 3 }
    my $v = 1;
    f($v);
    is $v, 3, 'a store to a sigilless parameter reaches its argument';
}

# --- a user .defined behind andthen/orelse writes an outer lexical ---
{
    my $calls = 0;
    my class O { method defined { $calls++; True } }
    my $o = O.new;
    my $r = ($o andthen 1);
    $r = ($o andthen 2);
    is $calls, 2, 'user .defined mutation is visible to the caller';
    is $r, 2, 'andthen result';
}

# --- a user sink writes an outer lexical ---
{
    my $sunk = 0;
    my class K { method sink { $sunk++ } }
    my $k = K.new;
    $k.self;
    $k.self;
    is $sunk, 2, 'user sink mutation is visible to the caller';
}

# --- but / does run a role TWEAK that writes an outer lexical ---
{
    my $invoked = 0;
    my role R { submethod TWEAK { $invoked++ } }
    my $z = Any.new;
    $z does R;
    ok $z ~~ R, 'does composed the role';
    ok $invoked >= 1, 'does-time TWEAK mutation is visible to the caller';
}

# --- eager gather writes an outer lexical, and a slot-only counter survives ---
{
    my $t = 0;
    for ^3 { my @e = eager gather { take 1; $t += 10 }; $t += 1 }
    is $t, 33, 'eager gather write and the loop counter both survive';
    my $was-lazy = 1;
    my @q = eager gather { $was-lazy = 0; take 5 };
    is $was-lazy, 0, 'eager gather write reaches the caller';
}
