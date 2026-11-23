use Test;

# `.match`'s `:c(N)` / `:pos(N)` start the scan at N together with `:g`, `:x`,
# `:nth`, `:ov` and `:ex`; `:pos` anchors only the first match (#18883).

plan 13;

is "abc".match(/./, :c(1), :g).map(~*).join, 'bc', ':c with :g';
is "abc".match(/./, :continue(1), :global).map(~*).join, 'bc', 'long adverb names';
is "abcabc".match(/b/, :c(2), :g).map(*.from).join(','), '4', ':c skips an earlier match';
is "aaa".match(/aa/, :c(1), :g).map(*.from).join(','), '1', 'a match covering the offset is not tried';
is "abc".match(/./, :c(1), :x(2)).map(~*).join, 'bc', ':c with :x';
is "abcd".match(/./, :c(1), :nth(2)).Str, 'c', ':c with :nth';
is "abcd".match(/./, :c(1), :ov).map(~*).join, 'bcd', ':c with :ov';
is "abcd".match(/./, :c(1), :ex).map(~*).join, 'bcd', ':c with :ex';
is "abc".match(/./, :c(5), :g).elems, 0, ':c past the end finds nothing';
is "abc".match(/./, :pos(1), :g).map(~*).join, 'bc', ':pos with :g';
is "abcabc".match(/c/, :pos(2), :g).map(*.from).join(','), '2,5', ':pos anchors only the first match';
is "abcabc".match(/c/, :pos(0), :g).elems, 0, ':pos with no match at the position finds nothing';
is "abc".match(/./, :g).map(~*).join, 'abc', 'without :c the scan starts at 0';
