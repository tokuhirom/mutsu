use Test;

# #9563: `my \foo = my $ = -3` binds `foo` to the new anonymous Scalar, so
# `foo = 5` writes through it. mutsu bound the plain value and died "Cannot
# modify an immutable Int". A named declarator (`my \foo = my $x = 3`)
# aliases `$x` the same way.

plan 11;

my \foo = my $ = -3;
foo = 5;
is foo, 5, 'a sigilless name bound to an anonymous Scalar is assignable';
foo .= abs;
is foo, 5, '... and takes a mutating method call';

my \neg = my $ = -3;
neg .= abs;
is neg, 3, '.= writes through the anonymous Scalar';

my \bar = my $x = 3;
bar = 7;
is $x, 7, 'a sigilless name bound to a named declarator aliases that variable';
bar++;
is $x, 8, '... including through ++';

my \a = my $ = 1;
my \b = my $ = 2;
a = 10;
is a, 10, 'two anonymous declarators in one scope are separate containers';
is b, 2, '... the second is unaffected by writes to the first';

sub counter() { my \n = my $ = 1; n = n + 1; n }
is counter(), 2, 'inside a routine';
is counter(), 2, '... each call gets a fresh container';

my \t = my Int $ = 4;
throws-like { t = "x" }, X::TypeCheck::Assignment,
    "the anonymous declarator's type constraint still applies";

my \r = 42;
throws-like { r = 1 }, Exception, 'a sigilless name bound to a plain value stays immutable';
