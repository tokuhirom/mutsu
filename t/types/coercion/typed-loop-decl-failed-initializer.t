use Test;

plan 5;

{
    my @seen;
    for ^2 {
        my Int $x = $_ == 0 ?? 7 !! die 'boom';
        CATCH { default { @seen.push: $x.raku } }
    }
    is-deeply @seen, ['Int'], 'a failed later initializer leaves the declared type object';
}

{
    my @seen;
    for ^1 {
        my Int $x = die 'boom';
        CATCH { default { @seen.push: $x.raku } }
    }
    is-deeply @seen, ['Int'], 'a failed first initializer leaves the declared type object';
}

{
    my @seen;
    for ^2 {
        my int $x = $_ == 0 ?? 7 !! die 'boom';
        CATCH { default { @seen.push: $x.raku } }
    }
    is-deeply @seen, ['0'], 'a native typed declaration resets to its native default';
}

{
    my Int $x = 9;
    my @seen;
    for ^2 {
        my Str $x = $_ == 0 ?? 'ok' !! die 'boom';
        CATCH { default { @seen.push: $x.raku } }
    }
    is-deeply @seen, ['Str'], 'a failed shadowing declaration has its own type object';
    is $x, 9, 'the outer binding survives the loop';
}
