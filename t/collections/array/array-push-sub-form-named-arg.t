use Test;

plan 12;

# The SUB forms of push/unshift/append/prepend take no named argument: the
# call matches neither of rakudo's candidates, `(\a, \b)` and `(\a, **@b is raw)`.
my @a = 1, 2;
my %h = i => 0;

sub nomatch(Str $code) {
    my $r = try { EVAL $code; 'ok' };
    $!.defined ?? ($!.^name, $!.message.lines.join('|')) !! ($r,)
}

my $cands = '|    (\a, \b)|    (\a, **@b is raw)';

is-deeply nomatch('push @a, :x(1)'),
    ('X::Multi::NoMatch', "Cannot resolve caller push(Array:D, :x(Int)); none of these signatures matches:$cands"),
    'push with a colonpair';
is-deeply nomatch('unshift @a, :x(1)'),
    ('X::Multi::NoMatch', "Cannot resolve caller unshift(Array:D, :x(Int)); none of these signatures matches:$cands"),
    'unshift with a colonpair';
is-deeply nomatch('prepend @a, f => 7'),
    ('X::Multi::NoMatch', "Cannot resolve caller prepend(Array:D, :f(Int)); none of these signatures matches:$cands"),
    'prepend with a fat arrow';
is-deeply nomatch('push @a, 5, :x(1)'),
    ('X::Multi::NoMatch', "Cannot resolve caller push(Array:D, Int:D, :x(Int)); none of these signatures matches:$cands"),
    'a positional before the named argument';
is-deeply nomatch('append %h, i => (1, 42)'),
    ('X::Multi::NoMatch', "Cannot resolve caller append(Hash:D, :i(List)); none of these signatures matches:$cands"),
    'a Hash target and a List value';
is-deeply nomatch('push my %hh, f => 7'),
    ('X::Multi::NoMatch', "Cannot resolve caller push(Hash:D, :f(Int)); none of these signatures matches:$cands"),
    'a declared Hash target';
is-deeply nomatch('push @a, f => [1, 2]'),
    ('X::Multi::NoMatch', "Cannot resolve caller push(Array:D, :f(Array)); none of these signatures matches:$cands"),
    'an Array value keeps its name';
is-deeply nomatch('push @a, f => 1..3'),
    ('X::Multi::NoMatch', "Cannot resolve caller push(Array:D, :f(Range)); none of these signatures matches:$cands"),
    'a Range value';
is @a.elems, 2, 'the failed calls pushed nothing';

# The method form ignores a named argument through its implicit *%_.
@a.push: :x(1);
is @a.elems, 2, 'method form with a colonpair pushes nothing';
@a.push(7);
is-deeply @a, [1, 2, 7], 'a positional push still works';
push @a, 8;
is-deeply @a, [1, 2, 7, 8], 'the sub form still works without named arguments';
