use Test;
# From Terminal::UI (Frame.make-height-computer): `@$heights[@changed[ $++ ]]++`.
plan 6;
my @i = 1, 2;
my $a = [0, 0, 0]; @$a[@i[$++]]++;
is-deeply $a, [0, 1, 0], 'postfix ++ evaluates a nested subscript once';
my $b = [0, 0, 0]; ++@$b[@i[$++]];
is-deeply $b, [0, 1, 0], 'prefix ++ evaluates a nested subscript once';
my $c = [5, 5, 5]; @$c[@i[$++]]--;
is-deeply $c, [5, 4, 5], 'postfix -- evaluates a nested subscript once';
my $n = 0; sub nx { $n++; 1 }
my $d = [0, 0]; @$d[nx()]--;
is $n, 1, 'side-effecting subscript called once';
my $e = [0, 0, 0]; my $r = @$e[$++]++;
is $r, 0, 'postfix result is the old value';
is-deeply $e, [1, 0, 0], 'and the first slot was bumped';
