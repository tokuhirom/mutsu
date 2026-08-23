use Test;

plan 3;

is IO::Spec::Unix.split('///').raku, 'IO::Path::Parts.new("","/","/")',
    'all-slash split keeps the root in both parts';
is IO::Spec::Unix.split('').raku, 'IO::Path::Parts.new("","","")',
    'empty split has three empty parts';
is IO::Spec::Unix.splitpath('.').raku, '("", "", ".")',
    'splitpath keeps a bare dot as the filename';
