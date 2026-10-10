# Reduced from the Green ecosystem distribution (module-scope Promise awaited
# from a sub in the module and from its END phaser).
unit module AwaitModuleLexical;

my $completion = Promise.new;

sub keep-it(Int $v) is export { $completion.keep($v) }
sub wait-it() is export { await $completion }

END {
    await $completion;
}
