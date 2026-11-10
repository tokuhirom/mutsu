use Test;
# #17376: `my \k = EXPR` marks its term readonly with one SetTermMark op; the
# mark must still be set after the store, cleared again on a re-declaration in
# a loop, and visible from closures and nested routines.
plan 10;
my $died = 0;
for ^3 {
    my \k = $_ * 2;
    try { k = 99; CATCH { default { $died++ } } }
}
is $died, 3, 'every iteration re-declares a readonly term';

my \x = 5;
throws-like { x = 6 }, Exception, 'assigning to a readonly term dies';
is x, 5, 'value unchanged';

sub inner { my \y = 7; my &c = { y + 1 }; c() }
is inner(), 8, 'closure reads the term';

sub twice { my \z = $^a; z * 2 }
is twice(4), 8, 'routine-body term';
is twice(5), 10, 'second call re-declares';

my @r;
for 1..3 -> $i { my \t = $i * 10; @r.push: t }
is @r, [10, 20, 30], 'loop terms';

my \bound := 3;
is bound, 3, 'bound term still works';
my $v = 4;
my \alias := $v;
alias = 9;
is $v, 9, 'a sigilless bind to a variable stays writable';

{
    my \w = 1;
    { my \w = 2; is w, 2, 'inner shadowing term' }
}
