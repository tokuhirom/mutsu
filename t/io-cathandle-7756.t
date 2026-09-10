use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 8;

my $first = make-temp-file(:content("foo bar"));
my $second = make-temp-file(:content("meow"));

# Every source boundary is a separator for line and word reads, even when the
# source files do not end with a newline.
is-deeply IO::CatHandle.new($first, $second).words.List,
    ("foo", "bar", "meow").List,
    'words treats a source boundary as a word boundary';
is-deeply IO::CatHandle.new($first, $second).lines.List,
    ("foo bar", "meow").List,
    'lines treats a source boundary as a line boundary';
my $get-cat = IO::CatHandle.new($first, $second);
is-deeply [$get-cat.get, $get-cat.get, $get-cat.get],
    ["foo bar", "meow", Nil],
    'get keeps source records separate';

is-deeply IO::CatHandle.new.slurp, Nil,
    'an empty CatHandle slurps to Nil';

my $binary = make-temp-file(:content(Buf.new(200, 201)));
is-deeply IO::CatHandle.new(:bin, $binary).slurp, Buf[uint8].new(200, 201),
    ':bin opens sources in binary mode';

my $switch = make-temp-file(:content("abcdef"));
my $switch-cat = IO::CatHandle.new($switch);
$switch-cat.encoding: Nil;
is-deeply $switch-cat.slurp, Buf[uint8].new("abcdef".encode),
    '.encoding: Nil switches an active CatHandle to binary mode';

my $midstream-cat = IO::CatHandle.new(make-temp-file(:content("abcdef")));
$midstream-cat.encoding: Nil;
is-deeply $midstream-cat.read(2), Buf[uint8].new("ab".encode),
    'binary mode reads raw bytes mid-stream';
$midstream-cat.encoding: 'utf8';
is $midstream-cat.readchars(2), 'cd',
    'encoding can switch back from binary mode mid-stream';
