use Test;

# ADR-12529 phase 0: one pin per row of the ADR's §2.1 table — where each kind
# of name resolves. Lexical names (variables, routines, `my` types, and
# anything EVAL or a symbolic lookup names) resolve through the code's lexical
# outer scope; dynamic variables and CALLER:: resolve through the callers.
#
# Every expectation was checked against rakudo. The `todo` tests are the ones
# mutsu gets wrong today because a frame's env is chained over its caller's
# env; each names the ADR phase expected to fix it, and that phase drops the
# `todo` when it does.

plan 27;

# --- ordinary lexicals and parameters ------------------------------------

{
    my $v = 'outer';
    my &c = -> { $v };
    sub call-it(&f) { my $v = 'caller'; f() }
    is call-it(&c), 'outer', 'a closure reads its own outer lexical, not its caller\'s same-named one';
}

{
    my $v = 'outer';
    {
        my $v = 'inner';
        is $OUTER::v, 'outer', 'OUTER:: reads the enclosing scope';
    }
}

{
    my $v = 'mine';
    is MY::<$v>, 'mine', 'MY:: reads the current scope';
}

{
    sub g() { LEXICAL::<$secret> // 'not visible' }
    sub h() { my $secret = 'caller lexical'; g() }
    is h(), 'not visible', 'LEXICAL:: in a callee does not see the caller\'s lexical';
}

# --- routine names --------------------------------------------------------

{
    sub foo() { 'definition scope' }
    sub g() { foo() }
    sub h() { my sub foo() { 'caller' }; g() }
    is h(), 'definition scope', 'a callee calls the routine visible where it is defined, not the caller\'s `my sub`';
}

{
    my &c = do { my sub bar() { 'closure outer' }; -> { bar() } };
    sub bar() { 'global bar' }
    is c(), 'closure outer', 'a closure keeps calling the `my sub` of the scope it closed over';
}

{
    sub named() { &?ROUTINE.name }
    is named(), 'named', '&?ROUTINE names the running routine';
}

# --- type names -----------------------------------------------------------

{
    sub g() {
        my $t = ::('SecretType');
        $t ~~ Failure ?? do { $t.so; 'not visible' } !! $t.^name
    }
    sub h() { my class SecretType { }; g() }
    is h(), 'not visible', 'a symbolic lookup in a callee does not find the caller\'s `my class`';
}

# --- dynamic variables and CALLER:: --------------------------------------

{
    sub g() { $*dyn // 'unset' }
    sub h() { my $*dyn = 'from caller'; g() }
    is h(), 'from caller', 'a dynamic variable resolves through the caller';
}

{
    my &c = do { my $*dyn2 = 'creation site'; -> { $*dyn2 // 'unset' } };
    sub h() { my $*dyn2 = 'call site'; c() }
    is h(), 'call site', 'a closure reads a dynamic variable from where it is called, not where it was created';
}

{
    sub g() { $CALLER::v }
    sub h() { my $v is dynamic = 'caller v'; g() }
    is h(), 'caller v', '$CALLER::x reads the caller\'s `is dynamic` lexical';
}

{
    sub g() { CALLER::<$*cv> // 'unset' }
    sub h() { my $*cv = 'caller dyn'; g() }
    is h(), 'caller dyn', 'CALLER::<$*x> reads the caller\'s dynamic';
}

{
    sub g() { DYNAMIC::<$*dv> // 'unset' }
    sub h() { my $*dv = 'dyn dv'; g() }
    is h(), 'dyn dv', 'DYNAMIC::<$*x> finds a dynamic through the callers';
}

# --- EVAL and symbolic lookup see the static scope (ADR §2.3) -------------

{
    my $v = 'outer v';
    my &c = -> { EVAL q[$v] };
    sub call(&f) { my $v = 'caller v'; f() }
    is call(&c), 'outer v', 'EVAL in a closure sees the closure\'s outer lexical, not the caller\'s';
}

{
    my $v = 'outer v';
    my &c = -> { EVAL q[$v] };
    is c(), 'outer v', 'EVAL in a closure called with no competing lexical sees its outer';
}

{
    sub g() { (try EVAL q[$secret]) // 'not visible' }
    sub h() { my $secret = 'caller lexical'; g() }
    is h(), 'not visible', 'EVAL in a sub does not see its caller\'s lexical (ADR §1.3)';
}

{
    my &mk = -> { -> { (try EVAL q[$hidden]) // 'not visible' } };
    sub k() { my $hidden = 'caller lexical'; mk() }
    is k()(), 'not visible', 'EVAL in a closure made by a callee does not see the caller\'s lexical (ADR §1.3)';
}

{
    sub g() { (try ::('$secret')) // 'not visible' }
    sub h() { my $secret = 'caller lexical'; g() }
    is h(), 'not visible', 'a symbolic ::(\'$x\') lookup in a callee does not see the caller\'s lexical';
}

{
    my $v = 'outer v';
    sub g() { (try ::('$v')) // 'not visible' }
    is g(), 'outer v', 'a symbolic lookup sees the routine\'s own outer lexical';
}

# --- closure capture (#12519) --------------------------------------------

{
    my &outer = do { my $cap = 'caller capture'; -> &make { make() } };
    sub make() { -> { (try EVAL q[$cap]) // 'not visible' } }
    is outer(&make)(), 'not visible', 'a closure made in a callee does not see the calling closure\'s capture';
}

{
    sub mk(&inner) { -> { inner() } }
    my $depth-value = 'lexical';
    my &f = mk(-> { $depth-value });
    sub deep($n) { $n == 0 ?? f() !! do { my $depth-value = 'dynamic'; deep($n - 1) } }
    is deep(5), 'lexical', 'a closure called at dynamic depth reads its lexical, not a caller\'s same-named one';
}

# --- a closure called inside a block of its creating routine (#18175) ----

{
    my $X = "outer";
    sub d() {
        my $outside = { $X };
        { my $X = "block"; $outside() }
    }
    is d(), 'outer', 'a closure reads its outer lexical, not a block of its caller that redeclares it';

    sub w() {
        my $writer = { $X = "written" };
        my $seen = { my $X = "block"; $writer(); $X }();
        "$seen $X"
    }
    is w(), 'block written', 'a closure writes its outer lexical, not the caller block\'s';
}

{
    class Inner18175 { method tag { "outer" } }
    sub b() {
        my $outside = { Inner18175.tag };
        my @t;
        {
            my class Inner18175 { method tag { "block" } }
            my $inside = { Inner18175.tag };
            @t.push: $outside(), $inside();
        }
        @t.join(",")
    }
    is b(), 'outer,block', 'a closure names its outer type, not a block of its caller\'s lexical class';
}

{
    sub f() { my &a = { b() ~ "!" }; my &b = { "b" }; a() }
    is f(), 'b!', 'a closure calls a code variable its scope declares after it';

    sub dyn() { my $c = { $*D18175 }; { my $*D18175 = "dyn"; $c() } }
    is dyn(), 'dyn', 'a dynamic variable still resolves through the caller';

    my $c;
    { my class Inner18175 { method tag { "block" } }; $c = -> Inner18175 $x { $x.tag } }
    dies-ok { $c(Inner18175) }, 'a closure parameter typed with a block class rejects the package class';
}

done-testing;
