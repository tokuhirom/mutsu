use Test;

plan 16;

# A variable *bound* to Nil (`my $v := Nil`) must reach the same method
# verdicts as the literal `Nil.foo` form. The named-receiver opcode used to
# skip all the Nil special-casing, so the warn-and-resume coercions silently
# absorbed to Nil (or returned "" without warning).

# --- numeric coercions warn "Use of Nil in numeric context", resume with zero
{
    my $v := Nil;
    is-deeply (quietly $v.Int), 0, 'bound-Nil .Int is 0';
    is-deeply (quietly $v.Numeric), 0, 'bound-Nil .Numeric is 0';
    is-deeply (quietly $v.Real), 0, 'bound-Nil .Real is 0';
    is-deeply (quietly $v.Num), 0e0, 'bound-Nil .Num is 0e0';
    is-deeply (quietly $v.Rat), 0.0, 'bound-Nil .Rat is 0.0';
    ok (quietly $v.Int).defined, 'bound-Nil .Int is a defined 0, not a type object';
    warns-like { my $x := Nil; $x.Int }, *.contains('Nil' & 'numeric'),
        'bound-Nil .Int warns about Nil in numeric context';
}

# --- the numifying Real methods do the same
{
    my $v := Nil;
    is-deeply (quietly $v.abs), 0, 'bound-Nil .abs is 0';
    is-deeply (quietly $v.round), 0, 'bound-Nil .round is 0';
}

# --- string coercion warns "Use of Nil in string context", resumes with ""
{
    my $v := Nil;
    is-deeply (quietly $v.Str), '', 'bound-Nil .Str is the empty string';
    warns-like { my $x := Nil; $x.Str }, *.contains('Nil' & 'string'),
        'bound-Nil .Str warns about Nil in string context';
    is-deeply (quietly $v.ords.elems), 0, 'bound-Nil .ords is an empty Seq';
}

# --- the methods Nil genuinely defines still dispatch normally
{
    my $v := Nil;
    is $v.WHAT.^name, 'Nil', 'bound-Nil .WHAT is Nil';
    is-deeply $v.defined, False, 'bound-Nil .defined is False';
    is $v.gist, 'Nil', 'bound-Nil .gist renders "Nil" without warning';
}

# --- an unknown method is still absorbed to Nil (Nil.FALLBACK)
{
    my $v := Nil;
    is $v.no-such-method.gist, 'Nil', 'bound-Nil unknown method absorbs to Nil';
}
