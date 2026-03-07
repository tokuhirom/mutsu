use Test;

plan 8;

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

my $fh := my class MyHandle is IO::Handle {
    has Buf[uint8] $.data .= new: "abc".encode;
}.new;

is-deeply $fh.data.decode, 'abc', 'typed has .= initializer works';
