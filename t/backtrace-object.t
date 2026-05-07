use Test;
plan 14;

# Basic: .backtrace returns a Backtrace object
try { die "oops" };
my $bt = $!.backtrace;
is $bt.^name, 'Backtrace', '.backtrace returns Backtrace object';

# .list returns a list of frames
my @frames = $bt.list;
ok @frames.elems >= 1, '.list returns at least one frame';

# Each frame is a Backtrace::Frame
my $f = @frames[*-1];
is $f.^name, 'Backtrace::Frame', 'frame is Backtrace::Frame';

# Frame has .subname, .file, .line
is $f.subname, '<unit>', 'outermost frame subname is <unit>';
ok $f.line > 0, 'frame .line is positive';
ok $f.file.chars > 0, 'frame .file is non-empty';

# .Str returns the text representation
my $str = $bt.Str;
ok $str.contains('in block <unit>'), '.Str contains "in block <unit>"';

# .gist returns "Backtrace(N frames)"
my $gist = $bt.gist;
ok $gist.contains('Backtrace('), '.gist starts with Backtrace(';
ok $gist.contains('frames)'), '.gist ends with frames)';

# say on backtrace uses gist
# (We just check gist format, not actual say output)
ok $gist ~~ /^ 'Backtrace(' \d+ ' frames)' $/, '.gist format is Backtrace(N frames)';

# Multi-frame backtrace
sub foo { die "inner" }
try { foo() };
my $bt2 = $!.backtrace;
my @frames2 = $bt2.list;
ok @frames2.elems >= 2, 'multi-frame backtrace has >= 2 frames';
is @frames2[0].subname, 'foo', 'first frame subname is foo';
is @frames2[*-1].subname, '<unit>', 'last frame subname is <unit>';

# Backtrace stringifies when used in string context
my $str2 = ~$bt2;
ok $str2.contains('in sub foo'), 'stringified backtrace contains "in sub foo"';
