use Test;

# A subrule body is matched against the WHOLE subject starting at the subrule's
# position, not against a `chars[pos..]` re-slice (ADR-0016 P1). Constructs that
# look at the text *before* the current position must therefore see the real
# preceding text, not a slice boundary. Before this, a subrule started a fresh
# coordinate system at its own start, so `<<` wrongly fired mid-word and
# look-behind wrongly saw "nothing before me".

plan 9;

grammar LeftWb {
    token TOP { 'ab' <t> }
    token t { << \w+ }
}
nok LeftWb.parse('abcd'), '<< inside a subrule does not fire after a word char';

grammar LeftWbOk {
    token TOP { 'ab ' <t> }
    token t { << \w+ }
}
ok LeftWbOk.parse('ab cd'), '<< inside a subrule still fires after a space';

grammar After {
    token TOP { 'ab' <t> }
    token t { <?after 'ab'> \w+ }
}
ok After.parse('abcd'), '<?after> inside a subrule sees text before the subrule';

grammar NotAfter {
    token TOP { 'ab' <t> }
    token t { <!after 'ab'> \w+ }
}
nok NotAfter.parse('abcd'), '<!after> inside a subrule sees text before the subrule';

grammar RightWb {
    token TOP { 'ab' <t> 'cd' }
    token t { >> \w+ }
}
nok RightWb.parse('abxxcd'), '>> inside a subrule does not fire mid-word';

# `^^` (start of line) used to work only via a thread-local carrying the char
# before the slice; keep it pinned now that the workaround is gone.
grammar Bol {
    token TOP { \n <ln> }
    token ln { ^^ \w+ }
}
ok Bol.parse("\nabc"), '^^ inside a subrule matches right after a newline';

grammar NotBol {
    token TOP { 'x' <ln> }
    token ln { ^^ \w+ }
}
nok NotBol.parse('xabc'), '^^ inside a subrule does not match mid-line';

grammar AtPos {
    token TOP { 'ab' <t> }
    token t { <at(2)> \w+ }
}
ok AtPos.parse('abcd'), '<at(N)> inside a subrule is a position in the whole subject';

# Absolute offsets on the produced Match node (unchanged behaviour, pinned
# because it is what the removed rebase used to establish by deep-copying).
grammar Span {
    token TOP { 'xx' <part> }
    token part { \w+ }
}
is Span.parse('xxyy')<part>.from ~ '..' ~ Span.parse('xxyy')<part>.to, '2..4',
    'a subrule Match keeps absolute .from/.to';
