use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

plan 3;

# The `use lib` path is computed, so the parse-time module scan cannot find
# the module: the block's import takes the run-time path (#14202).
sub outside { (try ::('BgeType').hi) // 'none' }
{
    use BlockGlobalExportType;
    is BgeType.hi, 'bge', 'the block sees the imported type';
    is outside(), 'none', 'a routine written outside the block does not';
}
is (try EVAL 'BgeType.hi') // 'gone', 'gone', 'a later EVAL does not either';
