use Test;

plan 8;

# An indexed bind shares the container with its source (bind, not assign).
my @a = 1, 2, 3;
my $x = 10;
@a[1] := $x;
$x = 20;
is @a[1], 20, 'array element bound to a scalar follows it';
@a[1] = 30;
is $x, 30, 'writing the bound element writes the source';

my %h;
my $y = 1;
%h<k> := $y;
$y = 5;
is %h<k>, 5, 'hash element bound to a scalar follows it';

# Chained bind and element-to-element bind.
my %p = a => 1;
my %q;
%q<b> := %p<a>;
%p<a> = 9;
is %q<b>, 9, 'element-to-element bind follows the source';

# `=>` is the bound value, not the bind result.
my %r;
my $k = 'v';
%r<z> := $k => 1;
is %r<z>.key, 'v', 'the pair is the bound value';
is %r<z>.value, 1, 'pair value';

# Expression context.
my @b;
my $c = (@b[0] := 7);
is $c, 7, 'indexed bind in expression context yields the value';

# Binding into an immutable subscript dies.
throws-like { EVAL '(1,2)[0] := 3' }, X::Bind, 'immutable target';
