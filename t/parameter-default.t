use Test;

plan 9;

# X::Parameter::Default: default value on a required parameter
throws-like 'sub f($x! = 3) { }', X::Parameter::Default,
    how => 'required', parameter => '$x';
throws-like 'sub f(:$x! = 3) { }', X::Parameter::Default,
    how => 'required';
throws-like 'sub f(@x! = [1]) { }', X::Parameter::Default,
    how => 'required', parameter => '@x';
throws-like 'sub f(%x! = {}) { }', X::Parameter::Default,
    how => 'required', parameter => '%x';

# X::Parameter::Default: default value on an anonymous slurpy parameter
throws-like 'sub f(*@ = 2) { }', X::Parameter::Default,
    how => 'slurpy', parameter => *.not;
throws-like 'sub f(*% = 2) { }', X::Parameter::Default,
    how => 'slurpy', parameter => *.not;

# X::Parameter::Default: default value on a named slurpy parameter
throws-like 'sub f(*@a = 2) { }', X::Parameter::Default,
    how => 'slurpy', parameter => '@a';

# Valid signatures must NOT throw
lives-ok { my &f = sub ($x = 3) { $x } }, 'optional positional default is fine';
lives-ok { my &g = sub (*@a) { @a.elems } }, 'slurpy without default is fine';
