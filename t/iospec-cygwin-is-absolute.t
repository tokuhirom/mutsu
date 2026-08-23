use Test;

plan 2;

ok IO::Spec::Cygwin.is-absolute('C:\\foo'),
    'Cygwin recognizes a Win32 drive path as absolute';
nok IO::Spec::Cygwin.is-absolute('foo'),
    'Cygwin keeps relative paths relative';
