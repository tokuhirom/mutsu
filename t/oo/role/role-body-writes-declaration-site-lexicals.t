use Test;

# A role body runs at composition, possibly inside a routine that composes the
# role, but the variables it names are those of the role's declaration site:
# each one is a shared cell the body runs against, so a write reaches the
# declaring block without a frame's return merge carrying it, and a composer's
# same-named lexical is left alone (ADR-12529 §3, slice 34). Checked against
# rakudo.

plan 6;

my $mixed = 0;
role Mixy { $mixed++; method m { 'm' } }
sub mix($n) { $n but Mixy }
mix(1);
is $mixed, 1, 'a role body composed inside a sub writes the declaration-site variable';
mix(2);
is $mixed, 1, 'and a second mixin of the same role is memoised';

my $count = 0;
role BaseType { $count++; method b { 'b' } }
sub mix-val($n) { $n but BaseType }
mix-val(1);
mix-val('x');
is $count, 2, 'each base type runs the body against the same variable';

my $seen = 'decl';
role Peek { $seen ~= '+'; method p { 'p' } }
sub composer($n) { my $seen = 'composer'; my $o = $n but Peek; $seen }
is composer(5), 'composer', "the composer's same-named lexical is not the body's";
is $seen, 'decl+', 'the body wrote the declaration-site variable';

my @log;
role Pusher { @log.push('composed'); method q { 'q' } }
sub mix-push($n) { $n but Pusher }
mix-push(1);
is-deeply @log, ['composed'], 'an array the body pushes to';
