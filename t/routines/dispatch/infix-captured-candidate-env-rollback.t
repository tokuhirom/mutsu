use v6;
use Test;

# A rejected candidate of an operator family that a module re-exports through a
# custom EXPORT must not leave its partially bound parameters behind in the
# caller's scope. Found by RedX::HashedPassword (Red's `infix:<eq>` family,
# re-exported as `Red::Operators::EXPORT::ALL::`): after `{ $obj.col eq 'x' }()`
# the caller's own `$a` held the rejected candidate's first argument (#12512).

plan 2;

my $lib = $*TMPDIR.add("mutsu-infix-rollback-{$*PID}");
$lib.mkdir;
LEAVE { try { .unlink for $lib.dir; $lib.rmdir } }

$lib.add('ROps.rakumod').spurt(q:to/END/);
    unit module ROps;
    role RAst is export { }
    class RThing does RAst is export { has $.n }
    multi infix:<eq>(RAst $a, RAst $b) is export { 'hit' }
    END

$lib.add('RFront.rakumod').spurt(q:to/END/);
    use ROps;
    sub EXPORT(|) {
        Map.new: |ROps::EXPORT::ALL::.pairs, 'RThing' => RThing
    }
    END

my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e', q:to/CODE/, :out, :err);
    use RFront;
    class Holder { method thing { RThing.new(n => 1) } }
    my $a = "mine";
    my $b = "mineb";
    my $h = Holder.new;
    my &f = { $h.thing eq 'x' };
    my $r = f();
    print "$r|$a|$b";
    CODE
my $out = $proc.out.slurp;
$proc.err.slurp;
is $out, 'False|mine|mineb', 'a rejected exported candidate leaves $a/$b alone';

my $proc2 = run($*EXECUTABLE, '-I', $lib.Str, '-e', q:to/CODE/, :out, :err);
    use RFront;
    print RThing.new(n => 1) eq RThing.new(n => 2);
    CODE
is $proc2.out.slurp, 'hit', 'a matching exported candidate still dispatches';
$proc2.err.slurp;
