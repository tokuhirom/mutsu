use Test;

# A whole-container assignment keeps the destination's descriptor: the
# source's name must not leak into the container it was copied into (#12638).
plan 8;

my @e; my @a = 1;
@e = @a;
is @e.name, '@e', 'assignment into an empty array keeps its own name';
is @a.name, '@a', 'the source keeps its name';

my @f = 1, 2;
@f = @a;
is @f.name, '@f', 'assignment over a filled array keeps its own name';

my @g;
@g = 5, 6;
is @g.name, '@g', 'a list assignment keeps the name';

my %h = a => 1;
my %g;
%g = %h;
is %g.name, '%g', 'assignment into an empty hash keeps its own name';
is %h.name, '%h', 'the source hash keeps its name';

my %k = b => 2;
%k = %h;
is %k.name, '%k', 'assignment over a filled hash keeps its own name';

my @c = (1, 2, 3);
@e = @c;
is-deeply @e.List, (1, 2, 3), 'contents still copied';
