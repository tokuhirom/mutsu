use Test;

# A sigilless alias to a NOT-YET-EXISTING element is a deferred vivification
# token: the element does not exist, but the binding denotes it, and a write
# through the name creates it. `MarkSigillessBindSource` settles a sigilless
# term's mutability from what the bind source denotes, and a token is not a
# container, so it used to answer "Cannot modify an immutable Package ((Any))".
# The `$`-sigil spelling already resolved the token correctly, which is what
# localised this to the sigilless verdict rather than to the token machinery.

plan 12;

# --- 1. a write through the alias vivifies ----------------------------------
{
    my @a;
    my \p := @a[5];
    p = 9;
    is-deeply @a, [Any, Any, Any, Any, Any, 9], 'an array alias vivifies on write';
}
{
    my %h;
    my \p := %h<x>;
    p = 9;
    is %h<x>, 9, 'a hash alias vivifies on write';
}
{
    my @a;
    my (\p) := (@a[5],);
    p = 9;
    is-deeply @a, [Any, Any, Any, Any, Any, 9], 'and so does one through a list literal';
}
{
    my %h;
    my \p := %h<a><b>;
    p = 9;
    is %h<a><b>, 9, 'a nested hash path vivifies the whole chain';
}

# --- 2. ... and reading it first does not ------------------------------------
{
    my @a;
    my \p := @a[5];
    is p, Any, 'the alias reads as Any before the write';
    is-deeply @a, [], 'and reading did not grow the array';
}
{
    my %h;
    my \p := %h<x>;
    is p, Any, 'the hash alias reads as Any too';
    is-deeply %h, {}, 'and the key was not created';
}

# --- 3. the `$`-sigil spelling, which already worked (controls) -------------
{
    my @a;
    my $p := @a[5];
    $p = 9;
    is-deeply @a, [Any, Any, Any, Any, Any, 9], 'control: the $-sigil array spelling';
}
{
    my %h;
    my $p := %h<x>;
    $p = 9;
    is %h<x>, 9, 'control: the $-sigil hash spelling';
}

# --- 4. an EXISTING element still aliases, and a VALUE is still immutable ----
{
    my @a = 1, 2;
    my \p := @a[0];
    p = 9;
    is-deeply @a, [9, 2], 'an existing element still writes through';
}
{
    my \lit := 5;
    dies-ok { lit = 9 }, 'a bind to a value is still immutable';
}
