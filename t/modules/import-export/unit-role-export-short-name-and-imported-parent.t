use v6;
use Test;

# From App::RaCoCo: `unit role A::B::Reporter is export;` publishes the short
# name `Reporter` to whoever `use`s it, and a `unit class A::B::Html does
# Reporter` / `is Base` in another module then names it by that short name.

plan 4;

my $dir = $*TMPDIR.child("mutsu-unit-export-{$*PID}");
$dir.child('Ue/Rep').mkdir;
END { try { for <Ue/Rep/Reporter Ue/Rep/Html Ue/Rep/Base Ue/Rep/Kid> { $dir.child("$_.rakumod").unlink }; $dir.child('Ue/Rep').rmdir; $dir.child('Ue').rmdir; $dir.rmdir } }

$dir.child('Ue/Rep/Reporter.rakumod').spurt(
    "unit role Ue::Rep::Reporter is export;\nmethod do() \{ ... \}\n");
$dir.child('Ue/Rep/Html.rakumod').spurt(
    "use Ue::Rep::Reporter;\nunit class Ue::Rep::Html does Reporter is export;\nmethod do() \{ 'html' \}\n");
$dir.child('Ue/Rep/Base.rakumod').spurt(
    "unit class Ue::Rep::Base is export;\nmethod hi() \{ 'base' \}\n");
$dir.child('Ue/Rep/Kid.rakumod').spurt(
    "use Ue::Rep::Base;\nunit class Ue::Rep::Kid is Base is export;\n");

sub run-snippet($source) {
    my $file = $dir.child('snippet.raku');
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, '-I', $dir.absolute, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $file.unlink;
    $out.trim
}

is run-snippet('use Ue::Rep::Html; say Ue::Rep::Html.new.do;'), 'html',
    'a unit class composes the imported unit role by its short name';
is run-snippet('use Ue::Rep::Html; say Ue::Rep::Html.new ~~ Ue::Rep::Reporter;'), 'True',
    'and does the role';
is run-snippet('use Ue::Rep::Reporter; say Reporter.^name;'), 'Ue::Rep::Reporter',
    'the importer sees the role under its short name';
is run-snippet('use Ue::Rep::Kid; say Ue::Rep::Kid.new.hi;'), 'base',
    'a unit class inherits from an imported class by its short name';

done-testing;
