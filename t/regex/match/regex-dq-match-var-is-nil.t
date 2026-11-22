use Test;

# Inside a regex, a `"..."` atom's `$0` / `$/` / `$<a>` name the regex's own
# `$/`, which is still Nil while it matches: the atom interpolates the empty
# string (with a "Use of Nil in string context" warning), as in rakudo
# (roast S05-mass/rx.t tests 127-134).

plan 6;

{
    CONTROL { when CX::Warn { .resume } }
    is ~('abxab' ~~ /(ab)x"$0"/), 'abx', '"$0" after a capture is empty';
    is ~('abxab' ~~ /(ab)"x$0"/), 'abx', 'inside a longer string';
    is ~('abxab' ~~ /x"$/"/), 'x', '"$/"';
    is ~('abxab' ~~ /x"$<a>"/), 'x', '"$<a>"';
    nok 'abxab' ~~ /(ab)x"$0" b/, 'so it is not a backreference';
    is ~('abx' ~~ /x $("")/), 'x', 'an empty $( ) interpolation matches the empty string';
}
