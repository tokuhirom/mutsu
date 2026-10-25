use Test;

plan 5;

# A bareword that names a type object is not a container: assigning to it
# dies as X::Assignment::RO, as a statement and inside an expression.

class C {}

throws-like { EVAL 'my $r = (C = C.new)' }, X::Assignment::RO, 'C = C.new in an expression';
throws-like { EVAL 'my $r = (C .= new)' }, X::Assignment::RO, '(C .= new) in an expression';
throws-like { EVAL 'say(C .= new)' }, X::Assignment::RO, 'C .= new as a call argument';
throws-like { EVAL 'say(C.=new)' }, X::Assignment::RO, 'C.=new as a call argument';
throws-like { EVAL 'C .= new' }, X::Assignment::RO, 'the statement form still dies';
