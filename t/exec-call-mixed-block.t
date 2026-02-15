use Test;
plan 2;

dies-ok { die "boom" }, 'dies-ok accepts block with named arg path', :todo(False);
ok True, 'execution continues after dies-ok with block arg';
