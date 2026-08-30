use Test;
plan 3;

my $todo-seen = True;
sub record-todo(:$todo) { $todo-seen = $todo }
record-todo :todo(False);
dies-ok { die "boom" }, 'dies-ok accepts a block in sink position';
ok !$todo-seen, 'a mixed statement call binds its named argument';
ok True, 'execution continues after dies-ok with block arg';
