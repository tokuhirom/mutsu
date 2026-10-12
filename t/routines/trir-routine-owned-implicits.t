use Test;

# A routine owns its `$_`, `$/` (with its capture views `$0`, `$<name>`) and
# `$!`: each starts unset in every call and never reads the caller's
# (ADR-12529 phase 3). A small routine with parameters runs on TRIR, which
# used to resolve these names as free variables through the caller's env.
# Every expectation was checked against rakudo.

plan 10;

sub topic-of($x) { $_ }
sub match-of($x) { $/ }
sub cap0-of($x) { $0 }
sub named-cap-of($x) { $<n> }
sub error-of($x) { $! }

$_ = 'caller topic';
is topic-of(1).raku, 'Any', 'a sub with a parameter does not read the caller topic';
given 'given topic' {
    is topic-of(1).raku, 'Any', '... nor the topic of a `given` around the call';
}
is (^3).map({ topic-of($_) }).map(*.raku).join(','), 'Any,Any,Any',
    '... nor the topic of a `.map` callback that calls it';

'abc' ~~ /(b)/;
is match-of(1).raku, 'Nil', 'a sub with a parameter does not read the caller $/';
is cap0-of(1).raku, 'Nil', '... nor the caller $0';
'abc' ~~ /$<n>=[c]/;
is named-cap-of(1).raku, 'Nil', '... nor the caller $<n>';

try { die 'boom' };
is error-of(1).raku, 'Nil', 'a sub with a parameter does not read the caller $!';

sub own-topic($x) { $_ = $x * 2; $_ }
is own-topic(21), 42, 'a sub writes its own $_';
is $_, 'caller topic', '... which does not reach the caller';

sub own-match($s) { $s ~~ /(\d+)/; ~$0 }
is own-match('ab12'), '12', 'a sub reads the $0 of its own match';
