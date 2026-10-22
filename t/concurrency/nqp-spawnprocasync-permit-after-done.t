use Test;
use nqp;

# A `nqp::permit` given AFTER the `done` event still reads what the child
# wrote: the output sits in the pipe until a reader takes it, as in MoarVM,
# where the pipe lives as long as its handle (#13513). The task used to be
# dropped from the process registry when `done` was pushed, so the late
# permit was a no-op and the stream produced no events at all.

plan 8;

my class Q is repr('ConcBlockingQueue') {}
my class TC is repr('AsyncTask') {}

# Events from `$q` as "name:arg|arg" strings, up to and including the first one
# for which `$stop(name, args)` holds (or a 5s sentinel, reported as 'timeout').
sub collect(Mu $q is raw, &stop) {
    my $sentinel := nqp::timer($q, nqp::hash(), 5000, 0, TC);
    my @events;
    loop {
        my $item := nqp::shift($q);
        if nqp::ishash($item) { @events.push: 'timeout'; last }
        my $name = nqp::atpos($item, 0);
        my @args;
        for 1 ..^ nqp::elems($item) -> $i {
            my $v := nqp::atpos($item, $i);
            @args.push: !nqp::isconcrete($v) ?? 'null'
                !! $v ~~ Blob ?? $v.^name ~ '=' ~ $v.decode.raku
                !! nqp::isint($v) ?? ($name eq 'ready' && $i == 2 ?? 'pid' !! nqp::unbox_i($v))
                !! nqp::isstr($v) ?? nqp::unbox_s($v)
                !! $v.^name;
        }
        @events.push: "$name:" ~ @args.join('|');
        last if stop($name, @args);
    }
    nqp::cancel($sentinel);
    @events.List
}

sub spawn(Mu $q is raw, $script, :$out, :$err) {
    my $cb := nqp::hash();
    nqp::bindkey($cb, $_, $_) for <done ready error>;
    nqp::bindkey($cb, 'stdout_bytes', 'out') if $out;
    nqp::bindkey($cb, 'stderr_bytes', 'err') if $err;
    nqp::bindkey($cb, 'buf_type', buf8.new);
    my $argv := nqp::list('sh', '-c', nqp::decont($script));
    nqp::spawnprocasync($q, '/bin/sh', $argv, nqp::cwd(), nqp::getenvhash(), $cb)
}

{
    my $q := nqp::create(Q);
    my $h := spawn($q, 'sleep 0.1; printf hello', :out);
    my @before = collect($q, -> $n, @a { $n eq 'done' });
    is @before.join(' ; '), 'ready:BOOTIntArray|pid ; done:0', 'the child is done before any permit';
    nqp::permit($h, 1, -1);
    my @after = collect($q, -> $n, @a { $n eq 'out' && @a[1] eq 'null' });
    is @after.join(' ; '), 'out:0|Buf[uint8]="hello"|null ; out:1|null|null',
        'a permit after done reads the buffered output and the end of the stream';
}

{
    my $q := nqp::create(Q);
    my $h := spawn($q, 'printf out; printf err >&2', :out, :err);
    collect($q, -> $n, @a { $n eq 'done' });
    nqp::permit($h, 2, -1);
    my @err = collect($q, -> $n, @a { $n eq 'err' && @a[1] eq 'null' });
    is @err.join(' ; '), 'err:0|Buf[uint8]="err"|null ; err:1|null|null', 'stderr can be permitted late on its own';
    nqp::permit($h, 1, -1);
    my @out = collect($q, -> $n, @a { $n eq 'out' && @a[1] eq 'null' });
    is @out.join(' ; '), 'out:0|Buf[uint8]="out"|null ; out:1|null|null', 'and stdout after it';
}

{
    my $q := nqp::create(Q);
    my $h := spawn($q, 'printf late', :out);
    collect($q, -> $n, @a { $n eq 'done' });
    nqp::permit($h, 1, 1);
    my @first = collect($q, -> $n, @a { $n eq 'out' });
    is @first.join(' ; '), 'out:0|Buf[uint8]="late"|null', 'a limited late permit reads that many chunks';
    nqp::permit($h, 1, -1);
    my @rest = collect($q, -> $n, @a { $n eq 'out' && @a[1] eq 'null' });
    is @rest.join(' ; '), 'out:1|null|null', 'a second permit reaches the end of the stream';
}

{
    # A child that is permitted in time behaves as before: output, then done.
    my $q := nqp::create(Q);
    my $h := spawn($q, 'sleep 0.2; printf now', :out);
    nqp::permit($h, 1, -1);
    my @events = collect($q, -> $n, @a { $n eq 'done' });
    is @events[*-1], 'done:0', 'an in-time permit still ends with done';
    ok @events.grep(*.starts-with('out:0|')).elems == 1, 'and the output came before it';
}
