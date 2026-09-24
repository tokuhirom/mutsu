unit module MutualRecursionCalleeFixture;

# `walk` and `step` call each other by bare name, like File::Directory::Tree's
# `rmtree` <-> `empty-directory`; both are exported, so both are import
# aliases in the importing EVAL.
my sub walk($n) is export { $n <= 0 ?? 'done' !! step($n) }
my sub step($n) is export { walk($n - 1) }

my sub run-walk($n = 3) is export { walk($n) }
