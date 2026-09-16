unit module GatherModuleLexicalHelper;

my sub helper($value) { $value * 2 }

sub gathered-double($value) is export {
    gather {
        take helper($value);
    }
}
