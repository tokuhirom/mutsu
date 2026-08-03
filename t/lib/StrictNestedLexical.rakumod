unit module StrictNestedLexical;

# A file-scope lexical of this compunit: it lives in the compunit-lexical store,
# not in the loading scope's `env`.
my $counter = 0;

sub inner() { $counter = $counter + 1 }

# Two levels deep: `inner` is reached from another routine of this module, not
# from the script, which is what used to lose the declaration.
sub bump() is export { inner(); $counter }
