use Test;
plan 1;

is_run 'say "hello world"', { out => { $_.contains("hello") } }, 'is_run supports block matcher in hash';
