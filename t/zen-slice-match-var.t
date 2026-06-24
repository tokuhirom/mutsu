use Test;

# `$<>` is the zen slice of the match variable `$/` (sugar for `$/<>`). It
# used to parse as a bare anonymous `$` variable followed by a `<>` zen-angle
# postfix, binding to an undefined anonymous state var instead of `$/`. After a
# `:g` global match `$/` is a list of Match objects, and `$<>` must expose them.

plan 6;

"a1b2c3" ~~ m:g/\w\d/;
is $<>.elems, 3, '$<> after :g match has all matches';
is $<>>>.Str.join(','), 'a1,b2,c3', '$<> yields each match';
is $/.elems, 3, '$/ also has 3 (sanity)';

# query-string style parse (the Humming-Bird Request.decode pattern)
my $path = '/search?q=raku&x=1';
my %query;
if $path ~~ m:g/\w+"="(<-[&]>+)/ {
    %query = Map.new($<>.map({ .split('=', 2, :skip-empty) }).flat);
}
is %query<q>, 'raku', 'query param q parsed via $<>';
is %query<x>, '1',    'query param x parsed via $<>';

# single match: $<> exposes the one match
"hello" ~~ /(\w+)/;
is $<>.Str, 'hello', '$<> on a non-:g match exposes the whole match';
