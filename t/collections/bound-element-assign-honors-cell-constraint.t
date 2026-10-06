use Test;

# After `%h<k> := $y` (or `@a[0] := $y`) the element IS `$y`'s container, so a
# plain element assignment through it obeys `$y`'s own `of` constraint and
# `is default`, not the aggregate's. Regression (#11810): the element store
# decayed `Nil` against the HASH's default and wrote the raw value into the
# shared cell, skipping `$y`'s type check.

plan 26;

# --- the type check -------------------------------------------------------------
{
    my Int $s = 1; my %g; %g<k> := $s;
    throws-like { %g<k> = "x" }, X::TypeCheck::Assignment,
        'a hash element bound to a typed scalar rejects a wrong-typed value';
    is $s, 1, 'the bound variable is untouched by the rejected store';
}
{
    my Int $u = 1; my @a; @a[0] := $u;
    throws-like { @a[0] = "x" }, X::TypeCheck::Assignment,
        'an array element bound to a typed scalar rejects a wrong-typed value';
    is $u, 1, 'the bound variable is untouched by the rejected store';
}

# --- a legal store still writes through the cell ---------------------------------
{
    my Int $s = 1; my %g; %g<k> := $s;
    %g<k> = 5;
    is $s, 5, 'a legal store through a bound hash element reaches the variable';
    is %g<k>, 5, 'and the element reads it back';
    my Int $p = 2; my @c = 0, 0; @c[1] := $p;
    @c[1] = 8;
    is $p, 8, 'a legal store through a bound array element reaches the variable';
    is-deeply @c.List, (0, 8), 'and the array reads it back';
}
{
    my Numeric $q = 1; my %n; %n<k> := $q;
    %n<k> = 2.5;
    is $q, 2.5, 'a subtype value passes the cell constraint';
    throws-like { %n<k> = "z" }, X::TypeCheck::Assignment, 'a non-Numeric does not';
}
{
    my Int $r = 1; my %m; %m<k> := $r;
    %m<k> += 4;
    is $r, 5, 'a compound assignment through the element updates the variable';
    %m<k>++;
    is $r, 6, 'an increment through the element updates the variable';
}

# --- the Nil default is the CELL's, not the aggregate's ---------------------------
{
    my Int $t = 1; my %h; %h<k> := $t;
    %h<k> = Nil;
    is $t.raku, 'Int', 'Nil through a bound hash element resets a typed scalar to its type object';
    my Int $v = 1; my @b; @b[0] := $v;
    @b[0] = Nil;
    is $v.raku, 'Int', 'the same for an array element';
}
{
    my Int $w is default(7) = 1; my %i; %i<k> := $w;
    %i<k> = Nil;
    is $w, 7, "Nil resets to the variable's own `is default`";
    my $y is default(3) = 9; my @b; @b[1] := $y;
    @b[1] = Nil;
    is $y, 3, 'an untyped scalar with `is default` resets to it through an array element';
}
{
    my %k is default(5); my Int $x = 1; %k<k> := $x;
    %k<k> = Nil;
    is $x.raku, 'Int', "the hash's own `is default` does not apply to a bound element";
    %k<other> = Nil;
    is %k<other>, 5, 'an ordinary element of the same hash still decays to it';
}
{
    my Int:D $d = 1; my %h; %h<k> := $d;
    throws-like { %h<k> = Nil }, X::TypeCheck::Assignment, 'Nil is not an Int:D';
    is $d, 1, 'the :D variable keeps its value';
}

# --- native element types wrap through the cell ------------------------------------
{
    my uint8 $n = 1; my @a; @a[0] := $n;
    @a[0] = 257;
    is $n, 1, 'a native-int cell wraps the stored value';
}

# --- an object hash keys the same way ---------------------------------------------
{
    my %o{Any}; my Str $t = "a"; %o{42} := $t;
    throws-like { %o{42} = 5 }, X::TypeCheck::Assignment, 'an object-hash element rejects a wrong type';
    %o{42} = "b";
    is $t, "b", 'and accepts a right one';
}

# --- elements that are NOT constrained cells are unaffected ------------------------
{
    my $plain = 1; my %j; %j<k> := $plain;
    %j<k> = "ok";
    is $plain, 'ok', 'a bound untyped scalar takes any value';
    my %i; %i<i> := 137;
    dies-ok { %i<i> = 5 }, 'an element bound to a literal stays read-only';
    my Int %typed; %typed<a> = 1;
    throws-like { %typed<a> = "x" }, X::TypeCheck::Assignment, 'a typed hash still checks its own elements';
}
