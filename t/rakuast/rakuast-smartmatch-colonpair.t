use experimental :rakuast;
use Test;

# A colonpair on the right of `~~` keeps how it was written: `$f.IO ~~ :!r`
# is a `ColonPair::False`, as in rakudo's RakuAST frontend, not the
# `r => False` it means (#14880, the `.DEPARSE` corpus's S16-filehandles/chmod.t).

plan 9;

sub rhs($src) { $src.AST.statements[*-1].expression.right }

isa-ok rhs(q{my $f; $f ~~ :!r}), RakuAST::ColonPair::False, ':!r';
isa-ok rhs(q{my $f; $f ~~ :r}), RakuAST::ColonPair::True, ':r';
isa-ok rhs(q{my $f; $f ~~ :r(1)}), RakuAST::ColonPair::Value, ':r(1)';
isa-ok rhs(q{my $f; $f ~~ r => 1}), RakuAST::FatArrow, 'a fat arrow stays one';
is q{my $f; $f ~~ :!e}.AST.statements[1].DEPARSE, '$f ~~ :!e', 'it deparses as written';

my $file = $*PROGRAM;
ok $file.IO ~~ :r, 'the smartmatch still runs: readable';
ok $file.IO ~~ :!d, 'not a directory';
ok 'no-such-file-here'.IO ~~ :!e, 'a missing file';
my %h = a => 1;
ok %h ~~ (a => 1), 'a pair against a hash';
