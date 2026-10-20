use Test;
use MONKEY-SEE-NO-EVAL;
plan 21;

my $message = 'Type check failed in binding to parameter \'key\'; expected Int but got Str ("a")';
for
    '%h<a> = 1',
    '%h<a> := 1',
    '%h<a>',
    '%h<a>:exists',
    '%h<a>:delete',
    '%h.AT-KEY("a")',
    '%h.ASSIGN-KEY("a", 1)',
    '%h.BIND-KEY("a", 1)',
    '%h.EXISTS-KEY("a")',
    '%h.DELETE-KEY("a")',
    '%h<a b> = 1, 2',
    '%h<a b> := (1, 2)',
    '%h<a b>',
    '%h<a b>:exists',
    '%h<a b>:delete',
    '%h.push("a" => 1)',
    '%h.append("a" => 1)',
    '%h = "a" => 1'
-> $operation {
    throws-like { EVAL 'my %h{Int}; ' ~ $operation },
        X::TypeCheck::Binding::Parameter, message => $message,
        "$operation binds the key parameter";
}

is EVAL('my %h{Int}; %h{1} = 2; %h{1}'), 2,
    'a correctly typed key still stores and reads';
is EVAL('my %h{Int}; my $x = 4; %h{1} := $x; $x = 5; %h{1}'), 5,
    'a correctly typed key still binds its source container';
throws-like { EVAL 'my Int %h{Int}; %h{1} = "a"' },
    X::TypeCheck::Assignment, 'a value type failure remains an assignment error';
