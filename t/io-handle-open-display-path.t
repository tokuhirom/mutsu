use Test;

plan 1;

is 'foo'.IO.open.Str, 'foo',
    'IO::Handle.open preserves the path as given for display';
