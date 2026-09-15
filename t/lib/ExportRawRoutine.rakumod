unit module ExportRawRoutine;

sub raw-export() is export is raw { 42 }
sub export-raw() is raw is export { 43 }
sub capture-export(Mu \type) is export is raw { type }
