use Test;

plan 8;

is IO::Handle.new.gist, 'IO::Handle<(IO)>(closed)',
    'a new handle renders its default IO path';
is IO::Handle.bless.gist, 'IO::Handle<(IO)>(closed)',
    'bless initializes the same default path';
is IO::Handle.new.raku.starts-with('IO::Handle.new(path => IO'), True,
    'the default path is stored in the handle';
is IO::Handle.new(:path(IO)).gist, 'IO::Handle<(IO)>(closed)',
    'an explicit IO type object uses its gist';
is IO::Handle.new(:path(IO::Path)).gist, 'IO::Handle<(Path)>(closed)',
    'a qualified path type object uses its short gist';
is IO::Handle.new(:path('example'.IO)).gist, 'IO::Handle<"example".IO>(closed)',
    'an explicit IO::Path keeps its path representation';

my $handle = IO::Handle.new(:path('example'.IO), :chomp, :encoding('utf8'));
is $handle.Capture.Hash<chomp>, True,
    'Capture retains native handle settings';
is $handle.Capture.Hash<encoding>, 'utf8',
    'Capture retains the encoding setting';
