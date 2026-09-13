# A module whose mainline declares a lexical (`my`) role and whose `sub EXPORT`
# reads it back. The role belongs to THIS compunit, so it must survive the EVAL
# that happens to trigger the first load: a re-`use` of an already-loaded module
# re-runs only `sub EXPORT`, never the mainline, so a torn-down role makes the
# second import die.
my role Marked {
    method marked-by { 'EvalModuleMyRole' }
}

sub EXPORT(|) {
    # Mixing the role in goes through the role registry, which is the record
    # that used to be torn down with the EVAL that triggered the first load.
    my $probe = 42 but Marked;
    die "lexical role lost" unless $probe.marked-by eq 'EvalModuleMyRole';
    Map.new
}
