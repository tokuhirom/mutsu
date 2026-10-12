use Test;
use nqp;

# `nqp::watchfile` (#11502, ADR-11502 §2.4.2): the Asynchronous op that pushes
# `[schedulee, name, renamed, error]` onto a `ConcBlockingQueue` for each
# change to a watched file or directory. Every expectation matches rakudo
# 2026.09.

plan 17;

my class Q is repr('ConcBlockingQueue') { }
my class TC is repr('AsyncTask') { }

# The next item on `q`, or 'timeout' if none arrives within `$ms`: a timer on
# the same queue bounds every wait, so a lost event fails, never hangs.
sub next-item(\q, $ms = 5000) {
    my $t := nqp::timer(q, 'timeout', $ms, 0, TC);
    my $item := nqp::shift(q);
    nqp::cancel($t);
    $item
}

my $dir = $*TMPDIR.add("mutsu-nqp-watchfile-$*PID");
$dir.mkdir;
LEAVE {
    .unlink for $dir.dir;
    $dir.rmdir;
}

{
    my $q := nqp::create(Q);
    my $file = $dir.add('a.txt');
    $file.spurt('x');
    my $handler := -> \name, \renamed, \err { };
    my $h := nqp::watchfile($q, $handler, ~$file, TC);
    is $h.^name, 'TC', 'the op answers a handle of the given type';
    is $h.REPR, 'AsyncTask', 'of REPR AsyncTask';
    sleep 0.2;
    $file.spurt('yy');
    my $item := next-item($q);
    is nqp::elems($item), 4, 'a change pushes a four-item array';
    ok nqp::atpos($item, 0) === $handler, 'the schedulee first';
    is nqp::unbox_s(nqp::atpos($item, 1)), 'a.txt', 'then the file name';
    is nqp::atpos($item, 1).^name, 'BOOTStr', 'as a BOOTStr';
    is nqp::unbox_i(nqp::atpos($item, 2)), 0, 'then 0 for a change';
    is nqp::atpos($item, 3).^name ~ nqp::isconcrete(nqp::atpos($item, 3)), 'BOOTStr0',
        'and no error: the BOOTStr type object';
    ok nqp::cancel($h) === $h, 'cancel answers the handle';
    sleep 0.2;
    $file.spurt('zzz');
    is next-item($q, 300), 'timeout', 'a cancelled watch reports nothing';
}

{
    my $q := nqp::create(Q);
    my $h := nqp::watchfile($q, -> \n, \r, \e { }, ~$dir, TC);
    sleep 0.2;
    $dir.add('b.txt').spurt('z');
    my $item := next-item($q);
    is nqp::unbox_s(nqp::atpos($item, 1)), 'b.txt', 'a watched directory reports the entry name';
    is nqp::unbox_i(nqp::atpos($item, 2)), 1, 'and 1 for a new entry';
    nqp::cancel($h);
}

{
    my $q := nqp::create(Q);
    nqp::watchfile($q, -> \n, \r, \e { }, ~$dir.add('missing'), TC);
    my $item := next-item($q);
    is nqp::atpos($item, 1).^name ~ nqp::isconcrete(nqp::atpos($item, 1))
        ~ nqp::atpos($item, 2).^name ~ nqp::isconcrete(nqp::atpos($item, 2)),
        'BOOTStr0BOOTInt0', 'a missing path pushes no name and no kind';
    is nqp::unbox_s(nqp::atpos($item, 3)), 'no such file or directory', 'but the error';
}

{
    my $q := nqp::create(Q);
    throws-like { nqp::watchfile($q, -> \n, \r, \e { }, ~$dir, Q) }, Exception,
        message => 'file watch result type must have REPR AsyncTask',
        'the handle type must be an AsyncTask';
    throws-like { nqp::watchfile(TC, -> \n, \r, \e { }, ~$dir, TC) }, Exception,
        message => 'file watch target queue must have ConcBlockingQueue REPR',
        'the target must be a queue';
}

# The supply form shares the watcher and still reports full paths.
{
    my $p = Promise.new;
    my $tap = IO::Notification.watch-path(~$dir).tap({ $p.keep($_) unless $p });
    sleep 0.2;
    $dir.add('c.txt').spurt('c');
    await Promise.anyof($p, Promise.in(5));
    is $p.result.path, ~$dir.add('c.txt'), 'watch-path still reports the joined path';
    $tap.close;
}
