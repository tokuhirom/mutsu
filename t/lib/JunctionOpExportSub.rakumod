# Uses JunctionOpCandidates for itself and has a `sub EXPORT` that exports none
# of its operators, as Red::Migration::Configuration does (#18478).
use JunctionOpCandidates;
sub EXPORT { Map.new }
