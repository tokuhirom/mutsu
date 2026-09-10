use Test;
plan 3;

my $x = 0;
my $todo-seen = True;
sub record-todo(:$todo) { $todo-seen = $todo }
record-todo :todo(False);
dies-ok { die "x" }, 'dies-ok via pair-encoded arguments';
ok True, 'dies-ok ran and returned';

ok !$todo-seen, 'a pair-encoded named argument reaches a user routine';
