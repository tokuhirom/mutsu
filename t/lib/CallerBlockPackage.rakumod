unit module CallerBlockPackage;

# Invoke a caller-supplied block from inside this module's package, the way
# `Test::Util`'s `group-of` invokes the block it was handed.
sub call-it(&blk) is export { blk() }
