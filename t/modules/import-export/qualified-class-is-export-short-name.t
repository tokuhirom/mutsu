use v6;
use Test;

# From App::Racoco::Report::ReporterCoveralls (ecosystem): `unit class
# A::B::MD5 is export;` makes the short name `MD5` visible to the importer.
plan 6;

my $dir = $*TMPDIR.add("mutsu-qexp-{$*PID}");
LEAVE { for <A/B/MD5.rakumod A/B/Blk.rakumod A/B/Tr.rakumod Mocks.rakumod A/B/Git.rakumod Mocks2.rakumod A/B/Fac.rakumod Usr.rakumod Usr2.rakumod Usr3.rakumod> { $dir.add($_).unlink }; }
$dir.add('A/B').mkdir;
$dir.add('A/B/MD5.rakumod').spurt: "unit class A::B::Other::MD5\n\tis export;\nmethod md5 \{ 'unit' }\n";
$dir.add('A/B/Blk.rakumod').spurt: "class A::B::Blk::Inner is export \{ method m \{ 'block' } }\n";

my $p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use A::B::MD5; print MD5.new.md5', :out;
is $p.out.slurp, 'unit', 'unit class with qualified name exports its short name';
$p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use A::B::Blk; print Inner.new.m', :out;
is $p.out.slurp, 'block', 'block class with qualified name exports its short name';

# A `unit module` class may inherit from the imported short name.
$dir.add('A/B/Tr.rakumod').spurt: "unit class A::B::Tr\n\tis export;\nmethod send \{ 's' }\n";
$dir.add('Mocks.rakumod').spurt: "use A::B::Tr;\nunit module Mocks;\nclass TM is Tr \{ method send \{ 'm' } }\n";
$p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use Mocks; print Mocks::TM.new.send', :out;
is $p.out.slurp, 'm', 'class in a unit module inherits from an imported short name';

# A second importer of an already-loaded module sees the short name too.
$dir.add('A/B/Git.rakumod').spurt: "use A::B::Tr;\nunit class A::B::Git is export;\n";
$dir.add('Mocks2.rakumod').spurt: "use A::B::Tr;\nunit module Mocks2;\nclass TM is Tr \{ method send \{ 'm2' } }\n";
$p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use A::B::Git; use Mocks2; print Mocks2::TM.new.send', :out;
is $p.out.slurp, 'm2', 'already-loaded module re-used by another unit module keeps the short name';

# `unit module A::B::Fac is export` publishes `Fac` for qualified sub calls.
$dir.add('A/B/Fac.rakumod').spurt: "unit module A::B::Fac is export;\nour sub make(--> Int) \{ 42 }\n";
$dir.add('Usr.rakumod').spurt: "use A::B::Fac;\nunit class Usr is export;\nmethod m \{ Fac::make }\n";
$p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use A::B::Fac; use Usr; print Fac::make, Usr.m', :out;
is $p.out.slurp, '4242', 'unit module with qualified name exports its short package name';

# ... and keeps working in a module's attribute default run from another module.
$dir.add('Usr2.rakumod').spurt: "use A::B::Fac;\nunit class Usr2 is export;\nhas \$.v = Fac::make;\n";
$dir.add('Usr3.rakumod').spurt: "use Usr2;\nunit class Usr3 is export;\nmethod v \{ Usr2.new.v }\n";
$p = run $*EXECUTABLE, '-I', $dir.Str, '-e', 'use Usr3; print Usr3.new.v', :out;
is $p.out.slurp, '42', 'short package name stays visible to the importing module\'s own code';
