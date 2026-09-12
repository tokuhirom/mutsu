use v6;
use Test;

plan 9;

use NativeCall :TEST;

is guess_library_name('libm.so.6', v1), 'libm.so.6',
    'the TEST export accepts the Str plus Version form';
is guess_library_name(('libm.so.6', v1)), 'libm.so.6',
    'the TEST export accepts the ABI-version List form';
is guess_library_name(-> { 'libm.so.6' }), 'libm.so.6',
    'the TEST export accepts a Callable library name';
like guess_library_name('libm.so.6'.IO), /'libm.so.6'$/,
    'the TEST export accepts an IO::Path library name';
is guess_library_name(Str), '',
    'an undefined Str type object has no library name';
like guess_library_name('m', v1), /'libm.so.' || '.dylib' || '.dll'/,
    'a library stem is decorated for the current platform';
throws-like {
    EVAL 'use NativeCall :NOSUCHTAG; 1';
}, X::Import::NoSuchTag, 'unknown NativeCall tags are still rejected';
my $proc = run($*EXECUTABLE, '-e', 'use NativeCall; say guess_library_name("m")',
    :out, :err);
$proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
nok $proc.so, 'guess_library_name is not exposed by the DEFAULT tag';
like $err, /guess_library_name/, 'the missing TEST import is reported';
