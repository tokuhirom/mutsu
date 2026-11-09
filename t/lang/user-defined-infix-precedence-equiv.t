use Test;
plan 6;

class N { has $.subj; has $.sel }
sub infix:<⪪>(Mu $l, Mu $r) is equiv(&infix:<x>) { N.new(:subj($l), :sel($r)) }
sub infix:<⥷>(Mu $l, Mu $r) is equiv(&infix:<xx>) { "($l ⥷ $r)" }

my $x = 5;
my $r = $x ⪪ 1 + 2;
is $r.^name, 'N', 'an operator equiv to x is looser than + (the outer operation)';
is $r.sel, 3, '+ bound first';
my $s = $x ⪪ 'a' ~ 'b';
is $s.^name, 'Str', 'and tighter than ~ (the ~ is the outer operation)';
is ('a' ⥷ 'b' ⥷ 'c'), '((a ⥷ b) ⥷ c)', 'left-associative like x';
is (1 + 2 ⥷ 3), '(3 ⥷ 3)', 'xx level: + binds first';
is (1 ⥷ 2 ~ 3), '(1 ⥷ 2)3', 'xx level: ~ is the outer operation';
