use Test;

# A value with roles mixed into it answers a catalog role's own rows ahead of
# its inner value's (the wrapper roles' `gist`), and only those: a user role
# and an allomorph's Str half change nothing.
plan 6;

my $broken = Promise.new;
$broken.break(X::AdHoc.new(payload => 'boom'));
my $err = do { try await $broken; $! };
like $err.gist, /'boom'/, 'the wrapped exception reads through the wrapper role gist';
ok $err.^roles.map(*.^name).grep(/Broken|Died/), 'a wrapper role is mixed into it';

role Mine { method m { 'mine' } }
my $x = 5 but Mine;
is $x.m, 'mine', 'a user role method answers';
is $x.gist, '5', 'a user role changes no builtin row';
is <42>.gist, '42', 'an allomorph gists as its numeric half';
is (7 but Mine).succ, 8, 'the inner value still answers its own methods';
