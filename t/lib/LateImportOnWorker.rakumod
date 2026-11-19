# Exports a routine for t/vm/scope/lexical-import-sets-shared-with-worker.t.
unit module LateImportOnWorker;
sub late-greet($who) is export { "hello $who" }
multi sub late-kind(Int) is export { 'int' }
multi sub late-kind(Str) is export { 'str' }
