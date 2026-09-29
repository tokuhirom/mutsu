use Test;

plan 4;

is [+].^name, 'Int', '[+].^name is a method call on the zero-operand reduction';
is [*].^name, 'Int', '[*].^name';
is [+].WHAT.gist, '(Int)', '[+].WHAT';
is ([+] 1, 2, 3).^name, 'Int', 'an operand-taking reduction still parses';
