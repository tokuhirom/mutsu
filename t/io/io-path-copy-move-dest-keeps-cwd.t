use v6;
use Test;

# From IO::Maildir: `.rename(.dirname.IO.add: "../cur/" ~ $uniq) given $.IO` renamed
# onto a destination resolved against the process cwd instead of the `CWD` the
# destination `IO::Path` carries, so a `chdir`-relative maildir never received the
# mail. A destination `IO::Path` resolves against its own `CWD`.
plan 7;

my $base = $*TMPDIR.add("mutsu-io-dest-cwd-{$*PID}-{(now.Real * 1000).Int}");
$base.mkdir;
LEAVE { try { run 'rm', '-rf', $base.Str } }
$base.add('one').mkdir;
$base.add('two').mkdir;
$base.add('one/a.txt').spurt('A');
$base.add('one/c.txt').spurt('C');
$base.add('one/m.txt').spurt('M');

# Not the process cwd, and relative: only the CWD attribute can place it.
my $dest-dir = IO::Path.new('two', :CWD($base.Str));

ok $base.add('one/a.txt').rename($dest-dir.add('a.txt')), 'rename onto a path with its own CWD';
ok $base.add('two/a.txt').e && !$base.add('one/a.txt').e, 'the file moved to the CWD-relative destination';

ok $base.add('one/c.txt').copy($dest-dir.add('c.txt')), 'copy onto a path with its own CWD';
ok $base.add('two/c.txt').e && $base.add('one/c.txt').e, 'the copy landed there and the source stays';

ok $base.add('one/m.txt').move($dest-dir.add('m.txt')), 'move onto a path with its own CWD';
ok $base.add('two/m.txt').e && !$base.add('one/m.txt').e, 'the file moved there';

my $moved = $base.add('two/a.txt').rename($base.add('two/b.txt').Str);
ok $moved && $base.add('two/b.txt').e, 'a plain absolute string destination still works';
