use Test;

# A module that merges another unit's GLOBAL into its own at BEGIN time
# (`GLOBALish.WHO.merge-symbols($handle.globalish-package)`, CompUnit::Util's
# `steal-globalish`) hands those symbols to whoever uses it, in the scope of
# that `use` (#15371). Checked against rakudo.

plan 3;

use lib $?FILE.IO.parent(2).sibling('lib').Str;

{
    use GlobalishThief;
    is StolenGlobalSymbol.hi, 'stolen', 'the merged symbol reaches the importer';
}
nok ::('StolenGlobalSymbol'), 'only in the scope of the use';

{
    use GlobalishThief;
    is ::('StolenGlobalSymbol').hi, 'stolen', 'by name as well';
}
