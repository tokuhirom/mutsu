# Exports a routine imported in a block by
# t/vm/scope/lexical-import-sets-shared-with-worker.t.
unit module LateImportOnWorkerBlock;
sub block-greet is export { 'from the block' }
