use v6;
use Test;

# `$*ARGFILES` is an IO::ArgFiles, a cat handle over the files in @*ARGS (or
# `$*IN` without arguments), and IO::CatHandle is an IO::Handle. A role mixed
# into it, parameterized or not, overrides `get`, reaches the cat's `get`
# with callsame, and sees each switch to the next file through `on-switch`.
# This is what IO::CatHandle::AutoLines (LN's `$*LN`) does. Same as raku.

plan 11;

ok IO::CatHandle ~~ IO::Handle, 'IO::CatHandle is an IO::Handle';
ok IO::ArgFiles ~~ IO::CatHandle, 'IO::ArgFiles is an IO::CatHandle';
is IO::ArgFiles.^mro.map(*.^name).join(' '), 'IO::ArgFiles IO::CatHandle IO::Handle Any Mu',
    'the MRO';

my $dir = $*TMPDIR.add("mutsu-argfiles-{$*PID}");
$dir.mkdir;
my @files = $dir.add('a'), $dir.add('b');
@files[0].spurt: "a\nb\n";
@files[1].spurt: "c\nd\ne\n";

sub run-it($code) {
    my $p = run $*EXECUTABLE, '-e', $code, |@files, :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    $err ?? "ERR: $err" !! $out
}

is run-it('say $*ARGFILES.^name; say $*ARGFILES ~~ IO::Handle'), "IO::ArgFiles\nTrue\n",
    '$*ARGFILES is an IO::ArgFiles and an IO::Handle';
is run-it('.say for lines()'), "a\nb\nc\nd\ne\n", 'lines() reads every file';

my $role = q:to/END/;
    use MONKEY-GUTS;
    role Counted[Bool:D :$reset = True] {
        has Int $.ln is rw = 0;
        has &!os;
        submethod TWEAK {
            return unless $reset;
            return unless self ~~ IO::Handle && self ~~ IO::CatHandle;
            sub zero { $!ln = 0 }
            &!os := &zero;
            nqp::bindattr(self, IO::CatHandle, '&!on-switch',
                Proxy.new(:FETCH{ &!os }, :STORE(-> $, $ { })));
        }
        method get { my \v = callsame; v === Nil ?? ($!ln = 0) !! ++$!ln; v }
    }
    END

is run-it($role ~ '$*ARGFILES does Counted; while $*ARGFILES.get -> $l { print "$l:{$*ARGFILES.ln} " }'),
    'a:1 b:2 c:1 d:2 e:3 ', 'a plain role: the count restarts per file';
is run-it($role ~ '$*ARGFILES does Counted[:reset]; while $*ARGFILES.get -> $l { print "$l:{$*ARGFILES.ln} " }'),
    'a:1 b:2 c:1 d:2 e:3 ', 'a parameterized role: the same';
is run-it($role ~ '$*ARGFILES does Counted[:!reset]; while $*ARGFILES.get -> $l { print "$l:{$*ARGFILES.ln} " }'),
    'a:1 b:2 c:3 d:4 e:5 ', 'without the reset the count runs on';
is run-it($role ~ '$*ARGFILES does Counted[:reset]; say $*ARGFILES.^name'),
    "IO::ArgFiles+\{Counted}\n", 'the mixin type of a named-only parameterization';

# The first source is open before `on-switch` is set, so the callback sees
# the switch to the second one and the end; it reads the cat's new state.
my $c = IO::CatHandle.new(|@files);
my @seen;
$c.on-switch = -> { @seen.push: $c.path ?? $c.path.basename !! 'none' };
Nil while $c.get.defined;
is @seen.join(','), 'b,none', 'on-switch on a plain cat sees each switch';
is run-it('my @s; $*ARGFILES.on-switch = -> { @s.push: $*ARGFILES.path ?? $*ARGFILES.path.basename !! "none" }; Nil while $*ARGFILES.get.defined; say @s.join(",")'),
    "b,none\n", '$*ARGFILES opens its first file on first use';

@files».unlink;
$dir.rmdir;
