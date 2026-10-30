# An EXPORT hook that counts its runs in the importer's dynamic `$*HOOK-RUNS`, for
# t/modules/import-export/eval-repeated-use-runs-hook-each-time.t.
sub EXPORT { $*HOOK-RUNS++; %() }
