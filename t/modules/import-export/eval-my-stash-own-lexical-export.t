use Test;
use lib 't/lib';

plan 4;

# `MY::` of an EVAL'd unit lists the `my sub ... is export` routines a module
# it `use`s declared, as the top level does, beside a re-exported stash's.
my $code = 'use ExportStashReexportPlusOwn; '
         ~ 'join ",", (so MY::<&own-plain>), (so MY::<&own-asserting>), (so MY::<&todo>)';
is EVAL($code), 'True,True,True', 'own lexical exports and a re-exported routine are in MY::';
is EVAL('use ExportStashReexportPlusOwn; own-plain()'), 'plain', 'the export is callable';
is EVAL('use ExportStashReexportPlusOwn; MY::<&own-plain>()'), 'plain', 'and callable through MY::';
is EVAL('use ExportStashReexportPlusOwn; so MY::.keys.grep(* eq "&own-asserting")'), 'True', 'listed in MY::.keys';
