use Test;

plan 2;

my @body-methods;
my $body-saw-composed;

unit class ClassBodyCompositionProbe;
has $.name;
@body-methods = ClassBodyCompositionProbe.^methods(:local).map(*.name).sort;
$body-saw-composed = ClassBodyCompositionProbe.^is_composed;

ok @body-methods.grep(* eq 'POPULATE'),
    'class body statements see the generated POPULATE method';
ok $body-saw-composed,
    'class body statements run after the class HOW composes the class';
