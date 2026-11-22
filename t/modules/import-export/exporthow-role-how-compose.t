use lib 't/lib';
use Test;

# A role declared through a custom role HOW (EXPORTHOW SUPERSEDE or a DECLAREd
# role-kind declarator) has the HOW's `compose` called once, with the role,
# and its `callsame` reaches the native role compose (#18081).

plan 6;

use SupersedeRoleCompose;
use DeclareRoleCompose;

role R[$x] { method hi { "hi $x" } }
class C does R[1] { }
role Q { method q { 'q' } }

is composed-roles().join(','), 'R,Q', 'the superseding HOW composed each role once';
is C.new.hi, 'hi 1', 'a parametric role still composes into a class';
is Q.new.q, 'q', 'a plain role still puns';
is composed-roles().join(','), 'R,Q', 'using the roles composes them no more';

traitrole T { method t { 't' } }
class D does T { }
is declared-composed().join(','), 'T', 'a DECLAREd role declarator composes its role';
is D.new.t, 't', 'and the role still composes into a class';
