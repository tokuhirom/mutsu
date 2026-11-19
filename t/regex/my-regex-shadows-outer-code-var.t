use Test;

# A `my regex` / `token` / `rule` declared in a routine is that routine's
# `&name`: an outer `my &name` of the same name does not shadow it (#17878).

plan 6;

my &r = sub { 'outer' };

sub mk() { my regex r { x }; &r }
isa-ok mk(), Regex, '&r in the routine is its own regex';

sub mk-token() { my token r { y }; &r }
my &rt = mk-token();
ok 'y' ~~ /<&rt>/, 'a token too';

sub mk5() { my $m = 'q'; my regex r { x $m }; &r }
my &r5 = mk5();
ok 'xq' ~~ /<&r5>/, 'the returned regex keeps its captured variable';

sub mk6() { my $m = 'q'; my regex r { x <?{ $m eq 'q' }> }; &r }
my &r6 = mk6();
ok 'x' ~~ /<&r6>/, 'a code assertion in the returned regex sees its routine\'s lexical';

is r(), 'outer', 'the outer &r is unchanged outside the routine';

sub no-own() { &r }
is no-own()(), 'outer', 'a routine without its own regex still sees the outer &r';
