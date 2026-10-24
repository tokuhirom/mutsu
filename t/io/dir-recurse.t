use v6;
use Test;

# `Rakudo::Internals.DIR-RECURSE` lists every file below a directory
# (mutsu#14906, Pakku::Meta's `bin/` walk).
plan 7;

my $root = $*TMPDIR.add( 'dir-recurse-' ~ $*PID );
END { run 'rm', '-rf', ~$root if $root.e }
for <a/b a/skip c> { $root.add($_).mkdir }
for <a/b/f1 a/f2 f3 c/.hid a/skip/gone> { $root.add($_).spurt: '' }

sub rel(@paths) { @paths.map({ .substr( $root.Str.chars + 1 ) }).sort.join(',') }

is rel( Rakudo::Internals.DIR-RECURSE(~$root) ), 'a/b/f1,a/f2,a/skip/gone,c/.hid,f3', 'every file, hidden ones included';
is rel( Rakudo::Internals.DIR-RECURSE(~$root, :file(*.ends-with('2'))) ), 'a/f2', ':file filters by basename';
is rel( Rakudo::Internals.DIR-RECURSE(~$root, :dir(* ne 'skip')) ), 'a/b/f1,a/f2,c/.hid,f3', ':dir filters the subdirectories entered';
is rel( Rakudo::Internals.DIR-RECURSE(~$root, :dir(*.ends-with('a')), :file(*.ends-with('1'))) ), '', 'the filters compose';
is Rakudo::Internals.DIR-RECURSE(~$root.add('nope')).elems, 0, 'a missing directory is empty';
isa-ok Rakudo::Internals.DIR-RECURSE(~$root).head, Str, 'the entries are Str paths';
is Rakudo::Internals.DIR-RECURSE(~$root.add('c')).head, $root.add('c').add('.hid').Str, 'prefixed with the directory given';
