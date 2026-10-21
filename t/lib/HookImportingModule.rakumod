# A module that imports a `sub EXPORT` hook's values for itself, at its own
# top level (#14140). Its routines use them; the program that `use`s this
# module does not get them.
unit module HookImportingModule;
use HookExportsValues;

sub module-reads-hook is export { hf() ~ ' ' ~ $hv ~ ' ' ~ @hl.join(',') }
