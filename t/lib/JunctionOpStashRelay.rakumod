# Re-exports JunctionOpCandidates' whole export stash from a `sub EXPORT`, so
# its operators do reach the importer (#18496).
use JunctionOpCandidates;
sub EXPORT { JunctionOpCandidates::EXPORT::DEFAULT:: }
