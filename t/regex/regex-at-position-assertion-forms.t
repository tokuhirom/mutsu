use Test;

# `<at(N)>` in all its spellings: capturing, non-capturing `<.at(N)>`,
# lookahead `<?at(N)>` and negated `<!at(N)>` (Red t/112).

plan 12;

for "aa", "xabc", "\na", "ba" -> $s {
    nok $s ~~ /^ <.at(1)> a/, "{$s.raku}: anchored at 0, so not at 1";
}
ok 'ba' ~~ /<.at(1)> a/, '<.at(1)> matches at position 1';
ok 'ba' ~~ /<?at(1)> a/, '<?at(1)> matches at position 1';
nok 'ba' ~~ /<!at(1)> a/, '<!at(1)> refuses position 1';
ok 'aa' ~~ /<!at(1)> a/, '<!at(1)> matches elsewhere';
is ('ba' ~~ /<at(1)> a/).Str, 'a', '<at(1)> matches at position 1';
is ('ba' ~~ /<!at(0)> a/).from, 1, '<!at(0)> skips position 0';
nok 'ab' ~~ /<at(5)> a/, 'a position past the end never matches';
ok 'a' x 300 ~~ /<at(256)> a/, 'a large position';
