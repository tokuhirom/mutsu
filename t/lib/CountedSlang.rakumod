# Fixture for t/modules/slang-module-loads-once.t: a slang that adds nothing
# and records each run of its mainline and of its `EXPORT` hook in the file
# `%*ENV<COUNTED_SLANG_LOG>` names.
our role CGrammar is export { }
our role CActions is export { }
%*ENV<COUNTED_SLANG_LOG>.IO.spurt("mainline\n", :append) if %*ENV<COUNTED_SLANG_LOG>;
use Slangify CGrammar, CActions;
