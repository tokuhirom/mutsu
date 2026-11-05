use Test;

# A `my enum` / `my constant` of a routine is that routine's lexical: a callee
# that EVALs the name must not find the CALLER's (ADR-12529, #16099).

plan 8;

sub reads-const { (try EVAL q[SECRETC]) // 'not visible' }
sub reads-lower { (try EVAL q[secretc]) // 'not visible' }
sub reads-enum  { (try EVAL q[Red]) // 'not visible' }

sub with-const { my constant SECRETC = 5; reads-const() }
is with-const(), 'not visible', 'an uppercase `my constant` of the caller';

sub with-lower { my constant secretc = 5; reads-lower() }
is with-lower(), 'not visible', 'a lowercase `my constant` of the caller';

sub with-enum { my enum Col <Red Green>; reads-enum() }
is with-enum(), 'not visible', 'a `my enum` key of the caller';

# The declaring routine and the program scope still see their own.
sub own-const { my constant OWNC = 7; EVAL q[OWNC] }
is own-const(), 7, 'the declaring routine reads its own constant through EVAL';

sub own-enum { my enum Dir <Up Down>; EVAL(q[Down]).Int }
is own-enum(), 1, 'the declaring routine reads its own enum key through EVAL';

constant PROGC = 11;
enum Level <Low High>;
sub reads-prog { EVAL q[PROGC] }
is reads-prog(), 11, 'a program-scope constant is visible to a callee EVAL';
sub reads-prog-enum { EVAL(q[High]).Int }
is reads-prog-enum(), 1, 'a program-scope enum key is visible to a callee EVAL';

sub caller-of-prog { my constant PROGC2 = 3; reads-prog() }
is caller-of-prog(), 11, 'a caller\'s other constant does not disturb the program-scope one';
