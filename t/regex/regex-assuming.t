use Test;

plan 5;

my $primed = /a/.assuming("xay");
isa-ok $primed, Sub, 'Regex.assuming returns a Sub';
is $primed.^name, 'Sub', '... whose type name is Sub';
is $primed.arity, 0, '... with no required arguments';
ok /a/.^can('assuming'), '.^can sees assuming on a regex';

my $none = /a/.assuming();
isa-ok $none, Sub, 'assuming with no arguments is a Sub as well';
