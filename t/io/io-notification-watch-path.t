use Test;

# #9586: `IO::Notification.watch-path` (and `IO::Path.watch`, its method form)
# emit `IO::Notification::Change` objects carrying the changed path and a
# `FileChangeEvent`. For a watched directory there is one event per changed
# entry, `.path` being the watched path joined with the entry name; creating
# or removing an entry is `FileRenamed`, changing one is `FileChanged`
# (rakudo's libuv mapping).

plan 21;

# The enum and the event class.
is FileChangeEvent.enums, Map.new((FileChanged => 1, FileRenamed => 2)), 'FileChangeEvent values';
is FileRenamed.^name, 'FileChangeEvent', 'bare enum value';
is FileChangeEvent::FileChanged, FileChanged, 'qualified enum value';

my $c = IO::Notification::Change.new(path => 'some/x', event => FileChanged);
is $c.path, 'some/x', 'Change.path';
is $c.event, FileChanged, 'Change.event';
is $c.gist, 'some/x: FileChanged', 'Change.gist is "path: event"';
isa-ok $c.IO, IO::Path, 'Change.IO is an IO::Path';
is $c.IO.basename, 'x', '... of the path';
is $c.raku, 'IO::Notification::Change.new(path => "some/x", event => FileChangeEvent::FileChanged)',
    'Change.raku';

my $dir = $*TMPDIR.add("mutsu-watch-path-$*PID");
$dir.mkdir;
my $outside = $*TMPDIR.add("mutsu-watch-path-src-$*PID");
LEAVE {
    .unlink for $dir.dir;
    $dir.rmdir;
    $outside.unlink if $outside.e;
}

# Drive a directory watch through create / write / remove, doing each step
# only once the previous one was reported. Consecutive events of one kind are
# folded: a single write may be observed in more than one poll.
sub drive(Supply $supply, IO::Path $entry) {
    my @kinds;
    my @paths;
    my @objects;
    my $timeout = Promise.in(10);
    react {
        whenever $supply -> $change {
            @objects.push: $change;
            @paths.push: $change.path;
            @kinds.push: $change.event unless @kinds && @kinds.tail === $change.event;
            given @kinds.elems {
                when 1 { $entry.spurt('changed content') }
                when 2 { $entry.unlink }
                when 3 { done }
            }
        }
        whenever Promise.in(0.2) {
            # A rename is atomic, so creation is one event.
            $outside.spurt('a');
            $outside.rename($entry);
        }
        whenever $timeout { done }
    }
    (@kinds, @paths.unique, @objects)
}

{
    my $entry = $dir.add('x');
    my ($kinds, $paths, $objects) = drive(IO::Notification.watch-path($dir.Str), $entry);
    is-deeply $kinds.List, (FileRenamed, FileChanged, FileRenamed),
        'watch-path: create, write, remove';
    is-deeply $paths.List, ("$dir/x",), '... reported under the watched path joined with the entry';
    ok $objects.all ~~ IO::Notification::Change, '... as IO::Notification::Change objects';
}

{
    my $entry = $dir.add('y');
    my ($kinds, $paths) = drive($dir.watch, $entry);
    is-deeply $kinds.List, (FileRenamed, FileChanged, FileRenamed), 'IO::Path.watch: create, write, remove';
    is-deeply $paths.List, ($dir.absolute ~ '/y',), '... reported under the absolute path';
}

# A trailing slash does not double up.
{
    my $entry = $dir.add('z');
    my ($kinds, $paths) = drive(IO::Notification.watch-path("$dir/"), $entry);
    is-deeply $paths.List, ("$dir/z",), 'watch-path("dir/") reports "dir/entry"';
}

# Watching a file reports the file itself.
{
    my $file = $dir.add('watched');
    $file.spurt('1');
    my @seen;
    my $timeout = Promise.in(10);
    react {
        whenever $file.Str.IO.watch -> $change {
            @seen.push: $change;
            done;
        }
        whenever Promise.in(0.2) { $file.spurt('grown') }
        whenever $timeout { done }
    }
    is @seen.elems, 1, 'a write to a watched file is reported';
    is @seen[0].event, FileChanged, '... as FileChanged';
    is @seen[0].path, $file.absolute, '... on the file itself';
    $file.unlink;
}

# Watching a missing path quits the supply.
{
    my $quit;
    my $timeout = Promise.in(10);
    react {
        whenever IO::Notification.watch-path($dir.add('missing').Str) {
            LAST { done }
            QUIT { default { $quit = $_; done } }
        }
        whenever $timeout { done }
    }
    isa-ok $quit, X::AdHoc, 'watching a missing path quits';
    is $quit.message, 'no such file or directory', '... with the libuv message';
}

# A closed tap receives nothing more.
{
    my @seen;
    my $tap = IO::Notification.watch-path($dir.Str).tap({ @seen.push: $_ });
    $tap.close;
    $dir.add('after-close').spurt('x');
    sleep 0.3;
    is @seen.elems, 0, 'no events after the tap is closed';
}
