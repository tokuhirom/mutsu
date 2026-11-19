use Test;

# `with` / `without` / `orwith` ask `.defined` of the value they tested
# exactly once, and the statement modifiers ask it of the topic. The probe is
# a plain method call on that value (#18208); everything the desugar exists
# for must still hold. Checked against rakudo.
plan 16;

{
    my $x = 3;
    with $x { $_ = 5 }
    is $x, 5, '`with $x { $_ = ... }` writes back to the variable';
}

{
    my %h = k => 1;
    with %h<k> { $_ = 7 }
    is %h<k>, 7, 'and to a hash element';
}

{
    my @got;
    with (1, 2, 3) -> @m { @got = @m.reverse }
    is @got.join(','), '3,2,1', '`with (...) -> @m` binds the list';
}

{
    my $calls = 0;
    sub probe-once() { $calls++; 42 }
    with probe-once() { }
    is $calls, 1, 'the condition is evaluated once';
}

{
    my @log;
    for Int, 5, Str, 'x' -> $v {
        with $v { @log.push: "def:$_" } else { @log.push: "undef:{.^name}" }
    }
    is @log.join(','), 'undef:Int,def:5,undef:Str,def:x', '`with` / `else` see the tested value';
}

{
    my $r = do with Int { 'a' } orwith 4 { "b$_" } else { 'c' };
    is $r, 'b4', '`orwith` takes the first defined value';
    my $e = do with Int { 'a' } orwith Str { 'b' } else { "c{$_.^name}" };
    is $e, 'cStr', 'an `else` after `orwith` sees the last tested value';
}

{
    my $f = Failure.new('nope');
    my $seen;
    with $f { $seen = 'defined' } else { $seen = 'undefined' }
    is $seen, 'undefined', 'a Failure is not defined';
    ok $f.handled, 'and asking marks it handled';
}

{
    class Never { method defined { False } }
    my $o = Never.new;
    my $taken = 'no';
    with $o { $taken = 'yes' }
    is $taken, 'no', 'a user `.defined` decides';
    without $o { $taken = 'without' }
    is $taken, 'without', 'for `without` too';
}

{
    sub first-or(@a) { return 'none' without @a[0]; "first:@a[0]" }
    is first-or([]), 'none', '`return ... without` returns';
    is first-or([9]), 'first:9', 'and falls through when defined';
}

{
    my @hits;
    @hits.push: "w$_" with $_ for 1, Int, 3;
    is @hits.join(','), 'w1,w3', 'a `with` modifier topicalizes the value';
}

{
    my $count = 0;
    sub hot($s) { my $n = 0; without $s.index('<', 1) { $n = 1 }; $n }
    $count += hot('a<b') + hot('abc') for ^500;
    is $count, 500, 'a hot `without` keeps answering';
}

{
    my $j = any(1, Int);
    my $r = 'none';
    with $j { $r = 'some' }
    is $r, 'some', 'a Junction condition autothreads `.defined`';
}
