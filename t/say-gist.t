use Test;
plan 2;

is_run 'say 1, 2', { out => "1 2\n" }, 'say joins args with space';
is_run 'say 1/3', { out => "<1/3>\n" }, 'say uses gist for Rat';
