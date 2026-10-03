use Test;

plan 6;

# A `.wrap` on a proto token's `:sym<..>` candidate, or on the proto itself,
# runs when the grammar dispatches through the proto (#11151).
my @log;
grammar P { token TOP { <p> }; proto token p {*}; token p:sym<a> { a } }
P.^find_method('p:sym<a>').wrap(-> |c { @log.push: 'wrapper'; callsame });
ok P.parse('a'), 'wrapped :sym candidate still matches';
is @log.join(','), 'wrapper', 'the candidate wrapper ran';

grammar Q { token TOP { <p> }; proto token p {*}; token p:sym<a> { a } }
my $proto = Q.^find_method('p');
ok $proto.defined, '.^find_method finds a proto token';
$proto.wrap(-> |c { @log.push: 'proto-wrapper'; callsame });
ok Q.parse('a'), 'wrapped proto still matches';
is @log.join(','), 'wrapper,proto-wrapper', 'the proto wrapper ran';

grammar R { token TOP { <p> }; proto token p {*}; token p:sym<a> { a }; token p:sym<b> { b } }
R.^find_method('p:sym<a>').wrap(-> |c { @log.push: 'ra'; callsame });
ok R.parse('b') && @log.tail ne 'ra', 'an unwrapped sibling candidate does not run the wrapper';
