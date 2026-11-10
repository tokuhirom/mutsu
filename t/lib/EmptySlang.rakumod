# Fixture for t/collections/hash/hash-elem-array-copies.t: a slang that adds
# nothing, so importing it only starts the slang host thread.
use v6.e.PREVIEW;
our role EGrammar is export { }
our role EActions is export { }
use Slangify EGrammar, EActions;
