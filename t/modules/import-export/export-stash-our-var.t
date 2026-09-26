use Test;
use lib 't/lib';

# `our` declarations directly inside a `package EXPORT::<tag> { ... }` block
# are that tag's exports -- the manual export-stash idiom `Cro::Uri` uses to
# re-export `decode-percents` from `Cro::ResourceIdentifier`.

plan 6;

{
    use ExportStashOurVar;
    is $answer, 42, 'an our scalar in EXPORT::DEFAULT is imported';
    is closure('x'), 'closure:x', 'an our code variable in EXPORT::DEFAULT is callable';
    nok (try EVAL 'helper-alias("x")'), 'a non-DEFAULT tag is not imported by default';
}

{
    use ExportStashOurVar :extra;
    is helper-alias('y'), 'helper:y', 'an our alias of a module-private sub is exported';
    is provided('z'), 'provided:z',
        "a re-export of a routine the module itself imported";
}

nok (try EVAL 'provided("w")'), "the module's own import stays out of scope here";
