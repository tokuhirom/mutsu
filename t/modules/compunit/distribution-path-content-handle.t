use v6;
use Test;

# `Distribution::Path.content` hands back an UNOPENED `IO::Handle` on the file
# (zef: `my $h = $dist.content($name-path); $h.close; $*REPO.load($h.path)`).
plan 10;

my $dir = $*TMPDIR.add('dist-path-content-' ~ $*PID);
END { run 'rm', '-rf', ~$dir if $dir.e }
$dir.add('lib').mkdir;
$dir.add('META6.json').spurt: '{"name":"X","version":"1","provides":{"X":"lib/X.rakumod"}}';
$dir.add('lib/X.rakumod').spurt: "unit module X;\n";

for ($dir, $dir.absolute.IO) -> $prefix {
    my $d = Distribution::Path.new($prefix);
    my $h = $d.content('lib/X.rakumod');
    isa-ok $h, IO::Handle, "content is an IO::Handle ({$prefix.is-absolute ?? 'absolute' !! 'relative'} prefix)";
    nok $h.opened, 'which is not open yet';
    ok $h.close, 'closing an unopened handle is fine';
    is $h.path.basename, 'X.rakumod', 'and its path names the file';
    is $d.content('lib/X.rakumod').open.slurp.chomp, 'unit module X;', 'reads once opened';
}
