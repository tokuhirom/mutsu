use Test;

plan 3;

is IO::Path::Win32.new('//server/share').basename, '\\',
    'UNC root basename uses the Win32 separator';
is IO::Path::Win32.new('C:\\foo/bar\\').gist, '"C:\\foo/bar\\".IO',
    'Win32 gist preserves a trailing separator';
is IO::Path::Win32.new('C:/').parent.gist, '"C:/".IO',
    'Win32 root gist preserves the input separator';
