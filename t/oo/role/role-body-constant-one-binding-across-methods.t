use v6;
use Test;

# Every method of a composed role reads one binding of the role body's
# `my constant`, also a method that contains a `use` (it runs in a module frame
# with its own import region) when the role comes from a loaded module. Hash::Util's
# `LockedHash` stores a `HIDDEN` sentinel in one method and compares against it
# in another (#16255).
plan 4;

my $dir = $*TMPDIR.add('role-const-binding-' ~ $*PID);
END { run 'rm', '-rf', ~$dir if $dir.e }
$dir.mkdir;
$dir.add('RoleConstBinding.rakumod').spurt: q:to/MODULE/;
    unit module RoleConstBinding;
    my role Holder {
        my constant HIDDEN = Mu.new but role { method defined { False } };
        method plain { HIDDEN.WHICH }
        method with-use { use nqp; HIDDEN.WHICH }
    }
    our sub mk(\h) is export { h does Holder }
    MODULE

sub run-script(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', ~$dir, '-e', 'use RoleConstBinding; ' ~ $code, :out, :err;
    $proc.out.slurp(:close).lines;
}

my @same = run-script('my %h; mk(%h); say %h.plain eq %h.with-use; say %h.with-use eq %h.plain');
is @same[0], 'True', 'a method with `use` sees the constant its sibling sees';
is @same[1], 'True', 'and the other way round';

# A second composition (another variable) still reads the role's one binding.
my @second = run-script('my %a; mk(%a); my %b; mk(%b); say %a.with-use eq %b.plain');
is @second[0], 'True', 'two compositions share it';

# The same inside a single file (no module frame involved).
my role Local {
    my constant TOKEN = Mu.new;
    method a { TOKEN.WHICH }
    method b { use nqp; TOKEN.WHICH }
}
my %l; %l does Local;
is %l.a, %l.b, 'a file-level role gives both methods one constant';
