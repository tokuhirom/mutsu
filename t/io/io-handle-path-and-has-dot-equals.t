use Test;

plan 7;

with IO::Handle.new(:path('foo')) {
    isa-ok .path, IO::Path, '.path turns Str :path to IO::Path';
    is-deeply .path.Str, 'foo', '.path has right value (Str :path)';
    isa-ok .IO, IO::Path, '.IO turns Str :path to IO::Path';
    is-deeply .IO.Str, 'foo', '.IO has right value (Str :path)';
    is-deeply .Str, 'foo', '.Str returns Str :path as Str';
}

with IO::Handle.new(:path('foo'.IO)) {
    is-deeply .path.Str, 'foo', '.path has right value (IO::Path :path)';
    is-deeply .Str, 'foo', '.Str returns IO::Path :path as Str';
}
