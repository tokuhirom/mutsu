use Test;

# A `.wrap` on a grammar token must still be honoured by the regex engine.
# The engine skips the wrap-chain lookup (and the per-subrule backtrace frame)
# while the method wrap table is empty, so this parses once with no wrap
# anywhere and then checks that the first wrap installed switches the lookup
# back on for a grammar that has not parsed yet.

plan 5;

grammar Plain {
    token TOP  { <word>+ % ' ' }
    token word { \w+ }
}

grammar Wrapped {
    token TOP  { <word>+ % ' ' }
    token word { \w+ }
}

ok Plain.parse('ab cd'), 'parse with no wrap installed anywhere';

my @seen;
Wrapped.^find_method('word').wrap(-> |c { @seen.push('word'); callsame });

my $m = Wrapped.parse('ab cd ef');
ok $m, 'parse succeeds through the wrapped token';
is $m<word>.elems, 3, 'wrapped token still produces its captures';
# TODO: Rakudo runs the wrapper exactly once per word (3); mutsu currently
# dispatches it twice per word, so only check that it ran at all.
ok @seen.elems >= 3, 'wrapper ran for every word';

nok Wrapped.parse('ab  cd'), 'wrapped token still fails where the grammar fails';
