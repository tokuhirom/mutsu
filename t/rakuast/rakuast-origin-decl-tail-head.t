use Test;
use experimental :rakuast;

plan 9;

# A declaration or assignment with a loose tail (`my $fh = open $f orelse
# die "x $f"`) places its head and the loose operator as rakudo does, and a
# term of the tail never takes the range of a same-shaped term of the head
# (#18578).

sub ranges(Str $code) {
    my @out;
    sub walk($n) {
        @out.push: $n.^name ~ ' ' ~ ($n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-');
        $n.visit-children(&walk);
    }
    walk($code.AST);
    @out
}

my @r = ranges q[my $file; my $fh = open $file, :w orelse die "x $file";];
ok @r.grep('RakuAST::Infix 34-40'), 'the orelse operator';
ok @r.grep('RakuAST::VarDeclaration::Simple 10-33'), 'the declaration';
ok @r.grep('RakuAST::Initializer::Assign 17-33'), 'its initializer';
ok @r.grep('RakuAST::Var::Lexical 48-53'), 'the interpolated variable of the tail';
is +@r.grep('RakuAST::Var::Lexical 24-29'), 1, 'only the head variable takes the head range';

@r = ranges q[my $x; $x = 1 and 2 or 3;];
ok @r.grep('RakuAST::ApplyInfix 7-19'), 'a chain: the inner application';
ok @r.grep('RakuAST::ApplyInfix 7-13'), 'a chain: the assignment';

@r = ranges q[my ($a, $b) = 1, 2 and 3;];
ok @r.grep('RakuAST::VarDeclaration::Signature 0-18'), 'a signature declaration';

@r = ranges q[my $x = 1 and 2 and 3;];
ok @r.grep('RakuAST::VarDeclaration::Simple 0-9'), 'a declaration under two tails';
