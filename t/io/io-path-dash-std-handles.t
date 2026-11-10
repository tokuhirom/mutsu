use Test;

plan 8;

{
    temp $*OUT = IO::Handle.new: :path(($*TMPDIR.add("mutsu-dash-out-{$*PID}.txt")));
    my $h = '-'.IO.open(:bin, :w);
    is-deeply $h, $*OUT, '"-".IO.open(:w) returns $*OUT (opening it when closed)';
    is-deeply $*OUT.opened, True, '$*OUT is now opened';
    is-deeply $*OUT.encoding, Nil, ':bin sets binary mode';
    '-'.IO.open: :enc<utf8-c8>, :w;
    is-deeply $*OUT.encoding, 'utf8-c8', ':enc changes the encoding';
    $*OUT.close;
    $*TMPDIR.add("mutsu-dash-out-{$*PID}.txt").unlink;
}

{
    my $in = $*TMPDIR.add("mutsu-dash-in-{$*PID}.txt");
    $in.spurt: "meows\n";
    temp $*IN = IO::Handle.new: :path($in);
    is-deeply '-'.IO.open(:bin), $*IN, '"-".IO.open returns $*IN';
    is-deeply $*IN.encoding, Nil, ':bin sets binary mode on $*IN';
    is-deeply $*IN.opened, True, '$*IN is now opened';
    $*IN.close;
    $in.unlink;
}

{
    my $p = run $*EXECUTABLE, '-e', q|print "-".IO.slurp|, :in, :out;
    $p.in.print("meows"); $p.in.close;
    is $p.out.slurp(:close), 'meows', '"-".IO.slurp reads $*IN';
}
