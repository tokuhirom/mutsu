use Test;
plan 3;

# A stale `$0` from an earlier match must not shadow the one `trans` publishes.
'"x"' ~~ /(\W)/;
is '"foo  &   bar"'.trans(
    /(' '+)/ => {' ' ~ ('&nbsp;' x ($0.chars - 1))},
    /(\W)/ => {"&#{ord($0)};"}
), '&#34;foo &nbsp;&#38; &nbsp;&nbsp;bar&#34;', 'closures read the match trans just made';

is '[36][38]'.trans(/\[(\d+)\]/ => {chr($0)}), '$&', 'chr($0) from the current match';
is 'aXb'.trans(/(X)/ => {"[$0]"}), 'a[X]b', 'interpolated $0';
