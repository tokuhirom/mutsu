use v6;
use Test;

# After an `s///`, `$/` must expose the positional captures ($0, $1, ...)
# just like a plain `m//` match does. Regression: mutsu built the post-`s///`
# `$/` from the match range only, dropping captures, so `$0`/`$1`/`$/[0]`
# came back empty. (Language/regexes.rakudoc)

plan 8;

{
    $_ = '2016-01-23 18:09:00';
    s/ (\d+)\-(\d+)\-(\d+) /today/;
    is $_, 'today 18:09:00', 's/// replaced the date';
    is "$0", '2016', '$0 holds the first capture after s///';
    is "$1", '01', '$1 holds the second capture after s///';
    is "$2", '23', '$2 holds the third capture after s///';
    is "$/[0]", '2016', '$/[0] indexes the capture after s///';
    is "$1-$2-$0", '01-23-2016', 'captures interpolate in a later string';
}

# :g leaves a list of Match objects in $/, each carrying its own captures.
{
    $_ = 'a1b2c3';
    s:g/(\w)(\d)/X/;
    is $_, 'XXX', ':g substitution replaced all three pairs';
    is $/.map({ .[0] ~ '=' ~ .[1] }).join(','), 'a=1,b=2,c=3',
        'each :g match keeps its own positional captures';
}
