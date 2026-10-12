use Test;

# The `$/` a `where m/.../` of the chosen multi method candidate set is visible
# in the body (PDF::Content::Image.load).

plan 3;

class K {
    multi method load(Str $s where m/^(\d)/ --> K:D) { $!seen = ~$0; self }
    multi method load(Str $s) { $!seen = 'other'; self }
    has $.seen;
}
is K.new.load('123abc').seen, '1', 'capture visible in the winning candidate';
is K.new.load('abc').seen, 'other', 'the fallback candidate';

class L {
    multi method m(Str $s where m/^(<alpha>+)(\d+)/) { "$0|$1" }
    multi method m(Int $i) { 'int' }
}
is L.m('ab12'), 'ab|12', 'several capture groups';
