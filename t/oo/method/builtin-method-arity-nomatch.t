use Test;

# A built-in method called with a positional count no candidate takes fails
# the way rakudo's dispatcher does (#16736): a multi whose proto binds the
# call raises X::Multi::NoMatch listing rakudo's candidates; a bounded proto
# (`proto method chars(*%)`) or an only method reports the arity.

plan 18;

sub failure(&code) {
    code();
    CATCH { default { return ($_.^name, $_.message) } }
    ('none', '');
}

{
    my ($name, $msg) = failure { "abc".lines("x", "y") };
    is $name, 'X::Multi::NoMatch', 'Str.lines with two positionals is a NoMatch';
    ok $msg.starts-with('Cannot resolve caller lines(Str:D: Str:D, Str:D); none of these signatures matches:'),
        'the header names the call profile';
    is $msg.lines.elems, 9, 'all eight Str/Cool candidates are listed';
    is $msg.lines[1], '    (Cool:D $:: *%_)', 'candidates follow rakudo dispatch order';
}

{
    my ($name, $msg) = failure { (1, 2).pick("a", "b") };
    is $name, 'X::Multi::NoMatch', 'List.pick with two positionals is a NoMatch';
    ok $msg.starts-with('Cannot resolve caller pick(List:D: Str:D, Str:D);'),
        'a List invocant is named List, not Array';
    is $msg.lines.elems, 7, 'the Any and List candidates are listed';
}

{
    my ($name, $msg) = failure { 5.base };
    is $name, 'X::Multi::NoMatch', 'Int.base with no argument is a NoMatch';
    is $msg.lines[0], 'Cannot resolve caller base(Int:D: ); none of these signatures matches:',
        'too few positionals is reported the same way';
}

{
    my ($name, $msg) = failure { 5.Array(1) };
    is $name, 'X::Multi::NoMatch', 'a one-candidate multi still raises NoMatch';
    is $msg.lines[1], '    ($:: *%_)', 'and lists its one candidate';
}

{
    my ($name, $msg) = failure { "abc".comb(1, 2, 3) };
    is $name, 'X::Multi::NoMatch', 'Str.comb with three positionals is a NoMatch';
    ok $msg.starts-with('Cannot resolve caller comb(Str:D: Int:D, Int:D, Int:D);'),
        'reached through the MRO walk too';
}

{
    my ($name, $msg) = failure { "abc".chars(1) };
    isnt $name, 'X::Multi::NoMatch', 'a bounded proto fails its own bind';
    is $msg, 'Too many positionals passed; expected 1 argument but got 2', 'with the proto arity';
}

{
    my ($name, $msg) = failure { "a".uc(1) };
    is $msg, 'Too many positionals passed; expected 1 argument but got 2', 'Str.uc likewise';
}

{
    my ($name, $msg) = failure { (1, 2).eager(1) };
    isnt $name, 'X::Multi::NoMatch', 'an only method (List.eager) is not a NoMatch';
    is $msg, 'Too many positionals passed; expected 1 argument but got 2', 'it reports its arity';
}
