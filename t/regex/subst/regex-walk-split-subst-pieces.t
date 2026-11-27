use Test;

# `.split(rx)` and `.subst(rx, :g)` walk one subject and build their result
# from the text between matches. The pieces are byte slices of the subject
# located from the engine's char positions, and a plain split builds no
# separator `Match` at all. These pin the pieces on multibyte subjects, the
# separators the `:v`/`:k`/`:p` adverbs still report, and `$/` after a global
# subst. Expected values checked against rakudo.

plan 19;

my $s = "日本 語x  é\tz";

is-deeply $s.split(/\s+/), ("日本", "語x", "é", "z").Seq, 'split on a multibyte subject';
is-deeply $s.split(/\s+/, 2), ("日本", "語x  é\tz").Seq, 'a limit keeps the rest whole';
is-deeply $s.split(/\s+/, :skip-empty), ("日本", "語x", "é", "z").Seq, ':skip-empty';
is-deeply $s.split(/<?before \S>/), ("", "日", "本 ", "語", "x  ", "é\t", "z").Seq,
    'zero-width separators';
is-deeply $s.split(/\s+/, :v).map(*.Str), ("日本", " ", "語x", "  ", "é", "\t", "z").Seq,
    ':v still reports each separator';
is-deeply $s.split(/(\s)\s*/, :v).map({ $_ ~~ Match ?? "[{.from},{.to},{.[0]}]" !! $_ }),
    ("日本", "[2,3, ]", "語x", "[5,7, ]", "é", "[8,9,\t]", "z").Seq,
    'a :v separator is a Match with real offsets and captures';
is-deeply $s.split(/\s+/, :k), ("日本", 0, "語x", 0, "é", 0, "z").Seq, ':k';
is-deeply $s.split(/\s+/, :p).map({ $_ ~~ Pair ?? "{.key}=>{.value.from}" !! $_ }),
    ("日本", "0=>2", "語x", "0=>5", "é", "0=>8", "z").Seq, ':p carries the Match';
is-deeply "".split(/x/), ("",).Seq, 'an empty subject';
is-deeply "abc".split(/<?>/), ("", "a", "b", "c", "").Seq, 'an empty separator';
is-deeply "a1b22c333".split(/\d+/), ("a", "b", "c", "").Seq, 'a trailing separator';
my $x = "1";
is-deeply "a1b22c1".split(/$x/), ("a", "b22c", "").Seq, 'an interpolated separator';
"a b".split(/\s/);
nok $/.defined, 'a plain split leaves $/ alone';

is "aé1日22c".subst(/\d+/, '#', :g), "aé#日#c", 'global subst keeps multibyte text';
is "aé1日22c".subst(/\d+/, '#'), "aé#日22c", 'single subst keeps multibyte text';
is "éé".subst(/x/, '#', :g), "éé", 'no match returns the subject';
is "日a日".subst(/<( a )>/, 'B', :g), "日B日", 'a narrowed match replaces only its region';
is "日本語".subst(/<?>/, "-", :g), "-日-本-語-", 'zero-width global subst';
"日1本22".subst(/\d+/, '#', :g);
is-deeply $/.map(*.Str), ("1", "22").Seq, '$/ after a global subst lists every match';
