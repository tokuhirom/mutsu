unit module ExportStashMod;
sub greet() is export { "hi" }
sub other() is export(:extra) { "x" }
