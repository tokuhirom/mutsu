unit module ExportClosureBlockImportInner;
sub ecbi-greet(:$from = "unset") is export { "inner greet from $from" }
sub ecbi-real(:$from = "unset") is export { "real from $from" }
