use v6;
use Test;

# From App::Racoco::Report::ReporterCoveralls (ecosystem): `unit class
# A::B::MD5 is export;` makes the short name `MD5` visible to the importer.
plan 2;

my $dir = $*TMPDIR.add("mutsu-qexp-{$*PID}");
LEAVE { for <A/B/MD5.rakumod A/B/Blk.rakumod> { $dir.add($_).unlink }; }
$dir.add('A/B').mkdir;
$dir.add('A/B/MD5.rakumod').spurt: "unit class A::B::Other::MD5\n\tis export;\nmethod md5 \{ 'unit' }\n";
$dir.add('A/B/Blk.rakumod').spurt: "class A::B::Blk::Inner is export \{ method m \{ 'block' } }\n";

my $p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use A::B::MD5; print MD5.new.md5', :out;
is $p.out.slurp, 'unit', 'unit class with qualified name exports its short name';
$p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use A::B::Blk; print Inner.new.m', :out;
is $p.out.slurp, 'block', 'block class with qualified name exports its short name';
