use v6;
use Test;
use lib 't/fixtures/await-module-lexical/lib';
use AwaitModuleLexical;

plan 1;

# `await $lexical` where the lexical is a module-scope `my` read from a
# routine in that module used to die with "No such method 'get-await-handle'
# for invocant of type 'Promise'" (the argument arrived as a container ref).
start { keep-it(42) };
is wait-it(), 42, 'await on a module-scope Promise lexical from a module sub';
