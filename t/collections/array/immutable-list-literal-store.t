use Test;

# A `List` is immutable as a CONTAINER: its element slots cannot be replaced.
# `(5, 6)[0] = 10` is "Cannot modify an immutable List ((5 6))" in rakudo, and
# mutsu enforced that only for the NAMED spelling (`my @t := (5, 6); @t[0] = 10`)
# — the anonymous one reached the generic auto-vivify store and silently
# succeeded.
#
# The refusal is keyed on the ELEMENT, not on the list: an element that IS a
# container stays writable through that container, which is what makes
# `my $a = 1; ($a, 6)[0] = 9` set `$a`.

plan 12;

sub msg(&c) { my $m; { c(); CATCH { default { $m = .message.lines[0] } } }; $m // '' }

# --- 1. a list of plain values refuses ---------------------------------------
like msg({ (5, 6)[0] = 10 }), /'Cannot modify an immutable List'/,
  'storing into an anonymous list literal is refused';
like msg({ (1, 2, 3)[1] = 9 }), /'Cannot modify an immutable List'/,
  '... at any index';
throws-like { (5, 6)[0] = 10 }, X::Assignment::RO,
  'and it is an X::Assignment::RO';
like msg({ my @t := (5, 6); @t[0] = 10 }), /'Cannot modify an immutable List'/,
  'control: the named spelling already refused';
like msg({ my $l := (5, 6); $l[0] = 10 }), /'Cannot modify an immutable List'/,
  'control: and the scalar-bound one';

# --- 2. ... while an element that is a CONTAINER stays writable --------------
{
    my $a = 1;
    ($a, 6)[0] = 9;
    is $a, 9, 'a list element that is a variable writes through';
}
{
    my $a = 1;
    my @l := ($a, 6);
    @l[0] = 9;
    is $a, 9, 'control: and so does the named spelling';
}

# --- 3. ... and a real Array is untouched ------------------------------------
{
    is ([5, 6][0] = 10), 10, 'a bracket array is mutable';
}
{
    my @a = 5, 6;
    @a[0] = 10;
    is-deeply @a.List, (10, 6), 'and so is an Array variable';
}
{
    my @a = 5, 6;
    my @b := @a;
    @b[1] = 9;
    is-deeply @a.List, (5, 9), 'a `:=` of an Array aliases it, and it is mutable';
}

# --- 4. reads are unaffected -------------------------------------------------
{
    is (5, 6)[0], 5, 'reading a list literal element still works';
    is-deeply (5, 6, 7)[1, 2].List, (6, 7), 'and so does a slice';
}
