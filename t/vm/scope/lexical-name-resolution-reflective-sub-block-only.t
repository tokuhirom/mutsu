use lib $?FILE.IO.parent(3).add('lib');
use Test;

# A mainline sub that names a lexical by string (`EVAL`, `::('$x')`) does not
# see a lexical declared only in the block that calls it, nor a variable a
# block-scoped `use` imported: neither is in the sub's lexical outer, which is
# the unit's top level (#13604, ADR-12529 phase 3).
plan 13;

sub via-eval { (try EVAL '$z') // 'not visible' }
sub via-symbolic { (try ::('$z')) // 'not visible' }
sub eval-write { try EVAL '$z = "written"'; }
sub through-plain { via-eval() }
sub later { (try EVAL '$later') // 'not visible' }
sub imported-eval { (try EVAL '$rsbi-v') // 'not visible' }
sub imported-symbolic { (try ::('$rsbi-v')) // 'not visible' }

if True {
    my $z = 'blk';
    is via-eval(), 'not visible', 'EVAL in a mainline sub does not see an if block\'s $z';
    is via-symbolic(), 'not visible', 'nor does ::(\'$z\')';
    is through-plain(), 'not visible', 'nor when called through another mainline sub';
    eval-write();
    is $z, 'blk', 'an EVAL write in the sub leaves the block\'s $z alone';
}
for 1 {
    my $z = 'loop';
    is via-eval(), 'not visible', 'a loop body\'s $z';
}
{
    my $z = 'bare';
    is via-symbolic(), 'not visible', 'a bare block\'s $z';
}

my $later = 'later';
is later(), 'later', 'a top-level my declared after the sub is visible';

{
    use ReflectiveSubBlockImport;
    is imported-eval(), 'not visible', 'EVAL does not see a block-scoped import';
    is imported-symbolic(), 'not visible', 'nor does ::(\'$x\')';
    is $rsbi-v, 'imported', 'the importing block sees the variable';
    is EVAL('$rsbi-v'), 'imported', 'and so does an EVAL in that block';
}
is imported-eval(), 'not visible', 'after the block the import is gone';

# A sub declared in a block keeps seeing that block's lexicals.
{
    my $own = 'own-block';
    sub in-block { EVAL '$own' }
    is in-block(), 'own-block', 'a block-declared sub sees its block\'s lexical';
}
