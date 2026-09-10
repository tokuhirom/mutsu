use Test;

# A sigilless bind whose SOURCE is itself a sigilless term takes on that term's
# binding: the shared container when it aliases one, the bare value when it was
# bound to one.
#
# `my \x := y` used to be refused outright — the parser's "can this RHS denote a
# container?" filter listed `Expr::Var`/`Index`/`MethodCall` but not a bareword,
# so a sigilless source took the static readonly path and `x = 5` died with
# "Cannot modify an immutable Int". The sigiled-target twin (`my $x := y`) has
# always worked, which is what localised it.

plan 14;

# --- 1. a chain through a named variable writes to the variable -------------
{
    my $a = 1;
    my \y := $a;
    my \x := y;
    x = 5;
    is $a, 5, 'a two-hop chain reaches the source variable';
}
{
    my $s = "q";
    my \m := $s;
    my \n := m;
    my \o := n;
    o = 42;
    is $s, 42, 'and so does a four-hop chain';
}
{
    my $b = 3;
    my \p := $b;
    is p, 3, 'the alias reads the source';
    $b = 4;
    is p, 4, 'and keeps reading it live';
}

# --- 2. a chain through an ELEMENT alias writes to the element --------------
{
    my @a = 1, 2;
    my \e := @a[0];
    my \f := e;
    f = 9;
    is-deeply @a, [9, 2], 'a chain through an array-element alias writes through';
}
{
    my %h = a => 1;
    my \g := %h<a>;
    my \h2 := g;
    h2 = 9;
    is %h<a>, 9, 'a chain through a hash-element alias writes through';
}
{
    my @a = 1, 2;
    my \e2 := @a[1];
    my \f2 := e2;
    is f2, 2, 'and the chained element alias reads the element';
}

# --- 3. ... while a chain rooted at a VALUE stays immutable -----------------
{
    my \lit := 5;
    my \z := lit;
    is z, 5, 'a chain rooted at a literal reads the value';
    dies-ok { z = 9 }, 'and refuses a write';
}
{
    my \lit2 := 5;
    my \z2 := lit2;
    my $err;
    { z2 = 9; CATCH { default { $err = .message } } }
    like $err, /'Cannot modify an immutable Int'/, 'naming the immutable value';
}
{
    constant K = 7;
    my \k := K;
    is k, 7, 'a chain rooted at a constant reads it';
    dies-ok { k = 9 }, 'and refuses a write';
}
{
    my \tn := Int;
    is tn.^name, 'Int', 'a bareword type name still binds the type object';
}

# --- 4. the sigiled-target spelling of the same chain (control) -------------
{
    my $a = 1;
    my \y2 := $a;
    my $x2 := y2;
    $x2 = 5;
    is $a, 5, 'control: `my $x := <sigilless>` still aliases';
}
