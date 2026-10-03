# The routine the wrapper module imports and re-exports a wrapper over.
unit module ExportOverride::Base;
our sub greet($x) is export { "base:$x" }
