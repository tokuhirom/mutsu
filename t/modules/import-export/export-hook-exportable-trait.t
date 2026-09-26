use v6;
use lib 't/lib';
use Test;

# The `Exportable` idiom (File::Stat, Color::DirColors): trait handlers
# declared `is export` inside `sub EXPORT`, recording routines into a hash the
# returned `&EXPORT` closure selects from by name.

plan 5;

{
    use ExportableTraitUser <pick-me>;
    is pick-me(1), 'picked 1', 'a routine selected through the inherited &EXPORT is imported';
}

{
    use ExportableTraitUser <pick-me leave-me>;
    is leave-me(2), 'left 2', 'every recorded routine can be selected';
}

{
    use ExportHookInnerExportSub;
    is from-hook(), 'declared inside sub EXPORT', 'an `is export` sub declared inside sub EXPORT is exported';
}

{
    sub r1 { 1 }
    trait_mod:<is>(&r1, :export);
    pass 'calling trait_mod:<is> with :export directly dispatches to the CORE candidate';
}

{
    my @seen;
    multi sub trait_mod:<is>(Routine:D \r, :$noted!) {
        @seen.push: r.name;
        trait_mod:<is>(r, :export($noted));
    }
    sub r2 is noted { 2 }
    is @seen, ['r2'], 'a custom trait that re-dispatches to :export runs to completion';
}
