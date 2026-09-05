use Test;

# An immutable container has no assignable element slot, and a `$`-sigil `:=`
# bind of one is not bound to a Scalar container either. Every assertion below
# is dual-oracled against rakudo.
plan 27;

# --- 1. a `$`-sigil `:=` bind of an immutable List ELEMENT ------------------
# rakudo: "Cannot assign to an immutable value" (X::AdHoc). The element of a
# List literal is a plain value, so the bind has no container to write through.
throws-like { my $x := (5, 6)[0]; $x = 10 }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'bind to an immutable List element refuses a later assignment';

# The sigilless twin has always refused; keep both spellings in agreement.
throws-like { my \x := (5, 6)[0]; x = 10 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Int (5)'/,
    'the sigilless spelling still refuses, naming the element';

# A real Array element IS a container, so the same bind writes through.
{
    my @a = 5, 6;
    my $x := @a[0];
    $x = 10;
    is @a[0], 10, 'a bind to an Array element still writes through';
}

# ... and so does an element that is itself a container inside a List literal.
{
    my $src = 1;
    my $x := ($src, 6)[0];
    $x = 10;
    is $src, 10, 'a List element that IS a container stays writable';
}

# A deferred (past the end) element bind still vivifies on write.
{
    my @a = 1, 2;
    my $r := @a[5];
    is @a.elems, 2, 'binding past the end does not grow the array';
    $r = 9;
    is @a.elems, 6, 'the first write through the bind grows it';
    is @a[5], 9, 'and lands in the right slot';
}

# --- 2. a `$`-sigil `:=` bind of a whole immutable container ----------------
throws-like { my $x := (1, 2, 3); $x = 5 }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'bind to a List literal refuses assignment';
throws-like { my $x := $(1, 2, 3); $x = 5 }, X::AdHoc,
    'bind to an itemized List refuses assignment';
throws-like { my $x := (1 .. 3); $x = 5 }, X::AdHoc,
    'bind to a Range refuses assignment';
throws-like { my $x := (1, 2, 3).Seq; $x = 5 }, X::AdHoc,
    'bind to a Seq refuses assignment';

# The bound name is still perfectly readable, and still indexable.
{
    my $l := (1, 2, 3);
    is $l[1], 2, 'a List-bound scalar still reads its elements';
    is $l.elems, 3, 'and still answers .elems';
}

# A bind to a mutable container is untouched.
{
    my @a = 1, 2;
    my $r := @a;
    $r.push(3);
    is @a.elems, 3, 'a bind to a real Array still aliases it';
}

# --- 3. a Seq element store ------------------------------------------------
# A Seq is not an assignable container: rakudo blames the ELEMENT.
throws-like { my $s = (1, 2, 3).Seq; $s[0] = 5 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Int (1)'/,
    'storing into a Seq element is refused, naming the element';
throws-like { my $s = ("a", "b").Seq; $s[1] = 5 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Str (b)'/,
    'the refusal names the element that was actually addressed';
throws-like { my $s = (1, 2, 3).Seq; $s[9] = 5 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Nil value'/,
    'a past-the-end Seq element is refused as Nil';
throws-like { my $s := (1, 2, 3).Seq; $s[0] = 5 }, X::Assignment::RO,
    'the `:=`-bound spelling refuses the same way';

# ... but an element that IS a container writes through, exactly like a List.
{
    my $x = 1;
    my $s = ($x, 2).Seq;
    $s[0] = 5;
    is $x, 5, 'a Seq element that is a container still writes through';
}

# The element-producing Seq shapes this path was built for stay writable.
{
    my @a = 1, 2, 3;
    my $s = @a.values;
    $s[0] = 9;
    is @a[0], 9, '@a.values keeps its producer element cells writable';
}

# Reading a Seq element is unaffected.
{
    my $s = (1, 2, 3).Seq;
    is $s[1], 2, 'a Seq element read still works';
}

# --- 4. the refusal message names the type and renders the gist ------------
throws-like { (1, 2, 3)[0] = 9 }, X::Assignment::RO,
    message => /'Cannot modify an immutable List ((1 2 3))'/,
    'a List store names the List and gists it';
throws-like { (1 .. 3)[0] = 9 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Range (1..3)'/,
    'a Range store names Range, not "value", and gists it';
throws-like { my $r = (1 .. 3); $r[0] = 9 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Range (1..3)'/,
    'the named Range spelling renders identically';
throws-like { my @t := (5, 6); @t[0] = 10 }, X::Assignment::RO,
    message => /'Cannot modify an immutable List ((5 6))'/,
    'the `@`-bound List spelling renders identically';
throws-like { $(1, 2, 3)[0] = 9 }, X::Assignment::RO,
    message => /'Cannot modify an immutable List ((1 2 3))'/,
    'an itemized List store renders as a List';

# A bracket array is a real mutable container and is untouched.
{
    my $a = [1, 2, 3];
    $a[0] = 9;
    is $a[0], 9, 'a bracket Array element store still works';
}
