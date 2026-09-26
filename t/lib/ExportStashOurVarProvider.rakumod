unit module ExportStashOurVarProvider;
our sub provided($s) is export(:provided) { "provided:$s" }
