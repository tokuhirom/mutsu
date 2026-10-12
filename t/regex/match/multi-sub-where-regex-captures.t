use Test;

# The `$/` a `where m/.../` of the chosen multi SUB candidate set is visible in
# the body (#12915), as it is for a plain sub and a multi method
# (multi-method-where-regex-captures.t). Values are rakudo's.

plan 5;

multi sub a(Str $s where m/^(\d)/) { 'a: ' ~ $0 }
multi sub a(Int $s) { 'int' }
is a('123'), 'a: 1', 'capture visible in the winning candidate';
is a(5), 'int', 'the other candidate';
is a('987'), 'a: 9', 'a second call sees its own match';

multi sub b(Str $s where m/^(<alpha>+)(\d+)/) { "$0|$1" }
multi sub b(Str $s) { 'plain' }
is b('ab12'), 'ab|12', 'several capture groups';
is b('!!'), 'plain', 'the fallback candidate';
