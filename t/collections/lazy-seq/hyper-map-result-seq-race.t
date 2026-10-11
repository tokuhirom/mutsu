use Test;

# From the App::Uni distribution: a `.hyper.map` callback that returns the Seq
# it just stored in a shared scalar (`$s .= grep(...)`) while a sibling item
# re-reads that scalar. The joining thread used to race the sibling's read and
# die with "The iterator of this Seq is already in use/consumed".

plan 2;

my $lock = Lock.new;
my $ok = True;
for ^20 {
    my $s = 0..30;
    try { (1, 2).hyper.map: -> $c { $lock.protect: { $s .= grep({ $_ %% 2 }) } } };
    $ok = False if $!;
    $ok = False unless $s.sort.elems == 16;
}
ok $ok, 'a Seq returned from a hyper.map callback is not raced by its siblings';

my $t = 0..30;
my @r = (1, 2).hyper.map: -> $c { (1, 2).grep(* == $c).Seq };
is @r.elems, 2, 'result Seqs of an ordinary hyper.map still reify';
