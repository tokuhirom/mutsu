use Test;

plan 1;

is 'Cargo.toml'.IO.open.Str, 'Cargo.toml',
    'IO::Handle.open preserves the path as given for display';
