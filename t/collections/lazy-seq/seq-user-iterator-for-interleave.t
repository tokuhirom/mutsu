use Test;

# A `for` over `Seq.new($iterator)` pulls one element per iteration, so the
# iterator's side effects interleave with the loop body (#15820, from LN's
# `.lines` line counter).

plan 7;

{
    my $i = 0;
    my class It does Iterator { method pull-one { $i < 3 ?? ++$i !! IterationEnd } }
    my @seen;
    for Seq.new(It.new) { @seen.push("$_/$i") }
    is-deeply @seen, ["1/1", "2/2", "3/3"], 'body sees the iterator state of its own pull';
}

{
    my $i = 0;
    my class Endless does Iterator { method pull-one { ++$i } }
    my @seen;
    for Seq.new(Endless.new) { @seen.push($_); last if $_ == 3 }
    is-deeply @seen, [1, 2, 3], 'last stops an unbounded user iterator';
    is $i, 3, 'no element past the last one was pulled';
}

{
    my $i = 0;
    my class It does Iterator { method pull-one { $i < 4 ?? ++$i !! IterationEnd } }
    my @pairs;
    for Seq.new(It.new) -> $a, $b { @pairs.push("$a$b/$i") }
    is-deeply @pairs, ["12/2", "34/4"], 'a two-parameter loop pulls two per iteration';
}

{
    my $n = 0;
    my class It does Iterator { method pull-one { $n < 3 ?? ++$n !! IterationEnd } }
    my $s = Seq.new(It.new);
    for $s { }
    is $s.list.elems, 3, 'the Seq stays readable after the loop';
}

# LN's `use LN` mixes a PARAMETERIZED role into $*ARGFILES (the mixin keeps the
# `Mixin` wrapper): `callsame` out of the role's `get` must reach the native
# `IO::Handle.get`, and a lazy `lines` Seq interleaves with the loop body.
{
    my $script = q:to/RAKU/;
        role Count[:$x] {
            has $.ln is rw = 0;
            method get { my \v = callsame; v === Nil ?? ($!ln = 0) !! ++$!ln; v }
            method lines {
                Seq.new: my class :: does Iterator {
                    has $!h;
                    method new(\h) { my $o = self.CREATE; $o.SET(h); $o }
                    method SET($h) { $!h := $h; self }
                    method pull-one { my \v = $!h.get; v =:= Nil ?? IterationEnd !! v }
                }.new(self)
            }
        }
        $*ARGFILES does Count[:x];
        for lines() { print "$_ ", $*ARGFILES.ln, " " }
        say $*ARGFILES.ln;
        RAKU
    my $proc = run $*EXECUTABLE, '-e', $script, :in, :out;
    $proc.in.print("foo\nbar");
    $proc.in.close;
    is $proc.out.slurp(:close).trim, 'foo 1 bar 2 0', 'parameterized-role mixin on $*ARGFILES: callsame + lazy lines';
    is $proc.exitcode, 0, 'exits cleanly';
}
