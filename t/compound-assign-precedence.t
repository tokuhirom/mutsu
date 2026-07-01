use Test;

plan 19;

# Word-form loose logical/flow compound assignment operators (`or=`, `and=`,
# `xor=`, `orelse=`, `andthen=`, `notandthen=`) are LIST-assignment operators:
# their RHS absorbs the whole comma list. The symbol forms (`||=`, `&&=`, `//=`,
# `^^=`) are item-assignment: tighter than the comma.

# --- item-assignment (symbol) forms: `$x op= a, b` == `($x op= a), b` ---
{
    my $x = 3;
    my @p = $x ||= 42, 43;
    is $x, 3, '||= is item assignment (LHS keeps 3)';
    is @p[0], 3, '||= item: @p[0] is scalar result';
    is @p[1], 43, '||= item: @p[1] is the trailing comma element';
}

{
    my $x;
    my @p = $x ^^= 42, 43;
    is $x, 42, '^^= is item assignment';
    is @p[0], 42, '^^= item: @p[0]';
    is @p[1], 43, '^^= item: @p[1]';
}

# --- list-assignment (word) forms: `$x op= a, b` == `$x op= (a, b)` ---
{
    my $a;
    my @p = $a or= 3, 4;
    is $a, (3, 4), 'or= is list assignment (RHS absorbs comma)';
    is @p[0][0], 3, 'or= list: @p[0] is the list (3,4)';
    is @p[0][1], 4, 'or= list: @p[0][1]';
}

{
    my $x;
    my @p = $x xor= 42, 43;
    is $x, (42, 43), 'xor= is list assignment';
    is @p[0][0], 42, 'xor= list: @p[0][0]';
    is @p[0][1], 43, 'xor= list: @p[0][1]';
}

{
    my $a = 3;
    my @p = $a and= 42, 43;
    is $a, (42, 43), 'and= is list assignment';
}

# short-circuit forms thunk (do not evaluate) their RHS when short-circuited
{
    my $thunky = 0; 1 or= $thunky++;
    is $thunky, 0, 'or= thunks its RHS';
    my $t2 = 0; 42 notandthen= $t2++;
    is $t2, 0, 'notandthen= thunks its RHS';
}

# --- item assignment `=` to a scalar is tighter than the comma ---
sub l () { 1, 2 }
{
    my $x;
    my @a = ($x = l(), 3, 4);
    is $x.elems, 2, 'scalar = is tighter than comma: $x gets just l()';
    is @a.elems, 3, 'scalar = is tighter than comma: @a is (($x=l()), 3, 4)';
}

# --- a scalar assignment used as an rvalue itemizes its value ---
{
    my $x;
    my @p = ($x = (3, 4));
    is @p.elems, 1, 'scalar assign result is itemized (one element)';
    is @p[0][0], 3, 'itemized element is the list (3,4)';
}
