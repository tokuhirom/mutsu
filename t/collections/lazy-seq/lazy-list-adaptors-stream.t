use Test;

# #9159: list methods and list operators over an infinite (lazy) input must
# stream, like Rakudo -- never throw "Cannot .X a lazy list", never return a
# silently truncated prefix (the old 1000-row `Z` / 256-element `X` caps).

plan 45;

# --- list methods over an infinite map pipe ---
is-deeply (1..*).map(* + 1).skip(2).head(2).List, (4, 5), '.skip over a lazy map';
ok (1..*).map(* + 1).skip(2).is-lazy, '.skip keeps it lazy';
is-deeply (1..*).map(-> $a, $b { $a + $b }).head(3).List, (3, 7, 11),
    'multi-arity .map pulls a chunk per call';
is-deeply (1..*).map(-> $a, $b { last if $a > 5; $a * $b }).head(5).List, (2, 12, 30),
    'multi-arity .map honours last';
is-deeply (1..*).map(* + 1).rotor(2).head(2).List, ((2, 3), (4, 5)), '.rotor over a lazy map';
is-deeply (1..*).map(* + 1).rotor(2 => -1).head(2).List, ((2, 3), (3, 4)),
    '.rotor with overlap';
is-deeply (1..*).map(* + 0).rotor(3 => -1, :partial).head(3).List,
    ((1, 2, 3), (3, 4, 5), (5, 6, 7)), '.rotor :partial over a lazy map';
is-deeply (1..*).map(* + 1).batch(2).head(2).List, ((2, 3), (4, 5)), '.batch over a lazy map';
is-deeply (^Inf).batch(2).head(2).List, ((0, 1), (2, 3)), '.batch over ^Inf';
is-deeply (1..*).map(* + 1).unique.head(2).List, (2, 3), '.unique over a lazy map';
is-deeply (1..*).map({ $_ % 3 }).unique.head(3).List, (1, 2, 0), '.unique drops repeats';
is-deeply (1..*).map(* + 0).unique(:with(&[==])).head(3).List, (1, 2, 3), '.unique :with';
is-deeply (1..*).map(* + 1).squish.head(2).List, (2, 3), '.squish over a lazy map';
is-deeply (1..*).map({ $_ div 3 }).squish(:as(* % 2)).head(3).List, (0, 1, 2), '.squish :as';
is-deeply (1..*).map(* div 2).repeated.head(2).List, (1, 2), '.repeated over a lazy map';
is-deeply (1..*).map({ ($_, $_) }).flat.head(4).List, (1, 1, 2, 2), '.flat over a lazy map';
is-deeply (1..*).map(* + 1).produce(&[+]).head(3).List, (2, 5, 9), '.produce over a lazy map';
is-deeply (1..*).map(* + 0).produce(-> $a, $b { $a ~ $b }).head(3).List, (1, "12", "123"),
    '.produce with a block';

# --- the same methods over a finite gather still reify it whole ---
is-deeply (gather { take 1; take 2; take 3 }).skip(1).List, (2, 3), '.skip over a gather';
is-deeply (gather { take 1; take 2; take 3 }).map(* + 1).skip(1).List, (3, 4),
    '.skip over a finite map pipe';

# --- Z and zip: no 1000-row cap ---
is-deeply ((1..*) Z (1..*))[1500], (1501, 1501), 'Z of two infinite ranges';
is ((1..*) Z+ (1..*))[2000], 4002, 'Z+ of two infinite ranges';
is-deeply ((1..*).map(* + 0) Z (1..*))[1500], (1501, 1501), 'Z of a lazy map and a range';
is ((1..*) Z+ (1..*) Z+ (1..*))[1500], 4503, 'n-ary Z+';
is-deeply zip((1..*), (1..*))[1500], (1501, 1501), 'zip() of two infinite ranges';
is zip((1..*), (1..*), :with(&[+]))[1500], 3002, 'zip(:with) of two infinite ranges';
ok ((1..*) Z (1..*)).is-lazy, 'Z of infinite operands is lazy';
nok ((1..3) Z (1..*)).is-lazy, 'Z with a finite operand is not lazy';
is ((1..5) Z (1..*)).elems, 5, 'Z is bounded by its finite operand';
is-deeply ((1, 2, *) Z (1..*)).head(4).List, ((1, 1), (2, 2), (2, 3), (2, 4)),
    'Z extends a trailing-* list';
is ((gather { take $_ for 1..2000 }) Z (1..*)).elems, 2000, 'Z of a long gather is not capped';

# --- X and cross: no 256-element cap ---
is-deeply ((1..*) X (1, 2))[600], (301, 1), 'X with an infinite left operand';
is-deeply cross((1..*), (1, 2))[600], (301, 1), 'cross() with an infinite operand';
is-deeply ((1, 2) X (1..*)).head(3).List, ((1, 1), (1, 2), (1, 3)),
    'X with an infinite right operand never advances the left';
is-deeply ((1..*) X~ <a b>).head(5).List, <1a 1b 2a 2b 3a>, 'X~ with an infinite operand';
is-deeply ((1..*) X (1, 2) X (3, 4)).head(3).List, ((1, 1, 3), (1, 1, 4), (1, 2, 3)),
    'n-ary X with an infinite operand';

# --- roundrobin: streams instead of panicking ---
is-deeply roundrobin((1..*), (5, 6)).head(3).List, ((1, 5), (2, 6), (3,)),
    'roundrobin with an infinite stream';
is-deeply roundrobin((1..*), (5, 6), :slip).head(5).List, (1, 5, 2, 6, 3),
    'roundrobin :slip with an infinite stream';

# --- xx *: no 4096-element (256 for a callable) prefix cap ---
is (42 xx *)[10**5], 42, 'xx * reaches past the old cache prefix';
ok (42 xx *).is-lazy, 'xx * is lazy';
is-deeply (|(1, 2) xx *).head(5).List, (1, 2, 1, 2, 1), 'a Slip LHS repeats its elements';
{
    my $n = 0;
    is ((rand, $n++)[1] xx *)[1000], 1000, 'a thunk LHS is re-evaluated per repetition';
}
{
    my @a = 0 xx *;
    is @a[5000], 0, 'an array assigned from xx * reaches index 5000';
}
is-deeply (Slip.new xx *).head(2).List, (), 'an empty Slip repeated forever is empty';

# --- rotor with an unbounded count cycle is not pre-expanded ---
is-deeply (1..20).rotor(1..*).map(*.elems).List, (1, 2, 3, 4, 5), 'rotor(1..*)';
