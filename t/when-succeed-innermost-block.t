use v6;
use Test;

plan 6;

# A `when`/`default` succeed exits its INNERMOST enclosing block only — not an
# outer `given`/`with`/`for`. DBIish's execute() computes each bind buffer with
# `do { when Blob {...} when Str {...} ... }` and then fills a MYSQL_BIND in a
# following `given` block; when the succeed leaked out of the `do`, the whole
# rest of the `with` body was skipped and every inserted value became NULL.

{
    my @out;
    for 'a', 42 -> $v {
        with $v {
            my $x = do {
                when Int { 'int' }
                when Str { 'str' }
                default  { 'other' }
            };
            @out.push($x);
        }
    }
    is @out.join('|'), 'str|int',
        'a matched when inside do {} exits only the do block';
}

{
    my $x = do given 42 { 'seed' };
    given 42 {
        $x = do { when Int { 'i' }; default { 'd' } };
    }
    is $x, 'i', 'the do block yields the matched when body value';
}

{
    my $reached = False;
    given 42 {
        my $x = do { when Int { 'i' } };
        $reached = True;
    }
    ok $reached, 'the given body continues after an inner do-when match';
}

{
    my $tail = False;
    given 5 {
        when Int { }
        $tail = True;
    }
    nok $tail, 'a when directly inside given still exits the given';
}

{
    my $tail = False;
    with 5 {
        when Int { }
        $tail = True;
    }
    nok $tail, 'a when directly inside with still exits the with';
}

{
    my @t;
    for 1, 2 {
        when 1 { @t.push('one') }
        @t.push("tail$_");
    }
    is @t.join('|'), 'one|tail2',
        'a when in a for body ends the iteration, not the loop';
}
