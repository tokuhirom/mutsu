use Test;

# A `rule` ends in an implicit `<.ws>` only when its body has whitespace
# before the closing `}`. A `:sym<...>` candidate of a `proto rule` follows
# the same rule as any other `rule` -- it used to get an unconditional
# trailing `<.ws>?` and swallow whitespace it never asked for (mutsu#9094).

plan 7;

grammar G {
    token x { \w+ }
    proto rule pf {*}
    rule pf:sym<n> {'not(' <x> ')'}
    proto rule ph {*}
    rule ph:sym<n> {:i'not(' <x> ')' }
    proto rule pk {*}
    rule pk:sym<n> {:i'not(' <x> ')'}
    rule ri {:i'not(' <x> ')'}
    rule rj {:i'not(' <x> ')' }
}

is ~G.subparse('not(a) b', :rule<pf>), 'not(a)',  'candidate without trailing space stops before whitespace';
is ~G.subparse('not(a) b', :rule<ph>), 'not(a) ', 'candidate with trailing space consumes whitespace';
is ~G.subparse('not(a) b', :rule<pk>), 'not(a)',  'candidate with :i and no trailing space';
is ~G.subparse('not(a) b', :rule<ri>), 'not(a)',  'plain rule without trailing space';
is ~G.subparse('not(a) b', :rule<rj>), 'not(a) ', 'plain rule with trailing space';

# The witness shape: the candidate must leave the space for the caller, so a
# following selector starts a new element instead of being absorbed.
grammar Sel {
    token TOP { <sel>+ % ' ' }
    token sel { '.'? <[a..z-]>+ <pf>? }
    proto rule pf {*}
    rule pf:sym<neg> {:i':not(' <sel> ')'}
}
my $m = Sel.parse('li:not(.pingback) .comment-content');
ok $m, 'selector list parses';
is $m<sel>.elems, 2, 'the space after the candidate separates two selectors';
