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

plan 21;

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
    todo 'ADR-12529 phase 2: routine names bind lexically';
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
    todo 'ADR-12529 phases 1 and 3: a `my class` is not a name a callee can see';
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
    todo 'ADR-12529 phase 3: EVAL resolves through the static chain';
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
    todo 'ADR-12529 phase 3: EVAL resolves through the static chain';
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

done-testing;
