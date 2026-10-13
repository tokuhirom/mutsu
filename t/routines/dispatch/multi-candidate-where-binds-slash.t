use Test;

plan 5;

# A `where` clause that smartmatches a regex sets `$/`, and the body of the
# multi candidate it selected sees that match, as for an only sub (rakudo
# binds the winning candidate's where clause in its own frame).
multi sub g(Str:D $ where .uc ~~ /$<y>=\d+/) { ~$<y> }
multi sub g(Int $) { 'int' }
is g('x12'), '12', 'multi sub: named capture from a where smartmatch';

my constant Re = rx/^ 'D:' $<year>=\d**4 /;
class C {
    multi method m(Str:D $ where $_ ~~ Re) { +$<year> }
    multi method m(Int $) { 'int' }
    multi method n(Str:D $ where { .lc ~~ /(\d)(\d)/ }) { "$0-$1" }
    multi method n(Int $) { 'int' }
}
is C.m('D:2015'), 2015, 'multi method: capture from a smartmatch against a constant regex';
is C.n('a34'), '3-4', 'multi method: positional captures from a block where clause';
is C.m(5), 'int', 'the other candidate still wins for its own type';

sub only(Str:D $ where .uc ~~ /$<z>=\d+/) { ~$<z> }
is only('q7'), '7', 'only sub: unchanged';
