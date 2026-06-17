use Test;

plan 12;

# Assigning to an undeclared dynamic variable throws X::Dynamic::NotFound
# (Raku semantics: a dynamic var must be declared with `my $*x` first).
{
    my $err;
    try { $*an_undeclared_dynvar = 42; CATCH { default { $err = $_ } } }
    ok $err.defined, 'assigning to undeclared $*var throws';
    is $err.^name, 'X::Dynamic::NotFound', 'correct exception type';
    is $err.message, 'Dynamic variable $*an_undeclared_dynvar not found',
        'correct message';
}

# A declared dynamic variable can be assigned freely.
{
    my $*declared = 1;
    $*declared = 2;
    is $*declared, 2, 'assigning to a declared $*var works';
}

# Assignment to a dynamic var declared in the caller writes through.
{
    sub setter() { $*shared = 5 }
    sub outer() { my $*shared = 99; setter(); $*shared }
    is outer(), 5, 'caller-declared dynamic var is assignable from callee';
}

# Built-in dynamic variables ($*OUT etc.) are in scope and assignable.
{
    my $saved = $*OUT;
    lives-ok { $*OUT = $saved }, 'built-in dynamic var $*OUT is assignable';
}

# @* and %* dynamic variables follow the same rule.
{
    my $err;
    try { @*undeclared_array = 1, 2, 3; CATCH { default { $err = $_ } } }
    is $err.^name, 'X::Dynamic::NotFound', 'undeclared @*var throws';
}
{
    my @*decl-arr = 1, 2, 3;
    @*decl-arr = 4, 5;
    is @*decl-arr.elems, 2, 'declared @*var is assignable';
}

# Reading an undeclared dynamic variable does NOT throw (returns undefined).
{
    lives-ok { my $v = $*never_declared_read; },
        'reading an undeclared $*var does not throw';
}

# A destructured signature parameter introduces a fresh dynamic var — binding it
# must NOT trip the not-found guard (regression: open.t/main.t).
{
    lives-ok {
        for (a => 1,) -> (:key(&*OPEN), :value($*PATH)) { my $x = $*PATH }
    }, 'destructured dynamic-twigil signature param binds without throwing';
}

# A dynamic var declared via `(my %*H)` then element-assigned works.
{
    (my %*OPTS)<flag> = True;
    is %*OPTS<flag>, True, 'element assign on a (my %*H)-declared dynamic hash';
}

# A plain dynamic-twigil sub param binds.
{
    sub takes(:value($*V)) { $*V }
    is takes(:value(7)), 7, 'dynamic-twigil named sub param binds';
}
