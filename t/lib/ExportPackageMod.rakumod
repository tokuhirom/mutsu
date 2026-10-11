unit module ExportPackageMod;
sub epm-foo is export { 42 }
sub epm-bar is export(:tag) { 7 }
