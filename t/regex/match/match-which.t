use v6;
use Test;

plan 8;

# Match.WHICH is reference identity (ObjAt), not the matched string's value
# identity (which delegated to Str and produced ValueObjAt.new("Str|...")).
"abc" ~~ /b/;
is $/.WHICH.^name, 'ObjAt', 'Match.WHICH is an ObjAt';
like $/.WHICH.raku, /^ 'ObjAt.new("Match|'/, 'Match.WHICH identity names Match';
ok $/.WHICH eqv $/.WHICH, 'same Match has a stable WHICH';

# A user-class instance likewise carries its class name, not `Any`.
class A { }
my $a = A.new;
is $a.WHICH.^name, 'ObjAt', 'instance WHICH is an ObjAt';
like $a.WHICH.raku, /^ 'ObjAt.new("A|'/, 'instance WHICH names its class';
ok $a.WHICH eqv $a.WHICH, 'same instance has a stable WHICH';
nok A.new.WHICH eqv A.new.WHICH, 'distinct instances have distinct WHICHes';

my $other = A.new;
"xy" ~~ /y/;
nok $/.WHICH eqv $other.WHICH, 'Match and unrelated instance differ';
