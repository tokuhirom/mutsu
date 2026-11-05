use Test;

plan 10;

# A regex built in a sub from that sub's lexicals keeps the interpolated value
# when it is the invocant of ACCEPTS or the needle of contains (#16691).
my sub tm(Str $g) { /$g/ }
is ("xabcx" ~~ tm("abc")).raku, 'Match.new(:orig("xabcx"), :from(1), :pos(4))', '~~ (already worked)';
ok "xabcx".match(tm("abc")).so, '.match (already worked)';
is tm("abc").ACCEPTS("xabcx").raku, 'Match.new(:orig("xabcx"), :from(1), :pos(4))',
    'Regex.ACCEPTS finds the match';
nok tm("zzz").ACCEPTS("xabcx").so, 'and a miss is a miss';
is "xabcx".contains(tm("abc")), True, 'Str.contains(Regex)';
is "xabcx".contains(tm("bc"), 3), False, 'with a start position past the match';
is "xabcx".contains(tm("bc"), 1), True, 'and one before it';
is "xabcx".contains(tm("zzz")), False, 'a miss';

my sub tn(Str $g) { my $h = $g; /$h/ }
ok tn("abc").ACCEPTS("xabcx").so, 'a copy of the argument interpolated';
my sub tl(Str $g) { /<{$g}>/ }
ok tl("abc").ACCEPTS("xabcx").so, 'a <{ }> interpolation';
