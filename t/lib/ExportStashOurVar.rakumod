use ExportStashOurVarProvider :provided;

sub helper($s) { "helper:$s" }

package EXPORT::DEFAULT {
    our $answer = 42;
    our &closure = sub ($s) { "closure:$s" };
}

package EXPORT::extra {
    our &helper-alias = &helper;
    our &provided = &ExportStashOurVarProvider::provided;
}
