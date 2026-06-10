use Test;

plan 8;

# Match-time adverbs are not allowed on rx// regex literals.
throws-like 'rx:g/a/',  X::Syntax::Regex::Adverb, adverb => 'g',  construct => 'rx';
throws-like 'rx:ov/a/', X::Syntax::Regex::Adverb, adverb => 'ov', construct => 'rx';
throws-like 'rx:ex/a/', X::Syntax::Regex::Adverb, adverb => 'ex', construct => 'rx';
throws-like 'rx:global/a/', X::Syntax::Regex::Adverb,
    adverb => 'global', construct => 'rx';

# :overlap / :exhaustive make no sense on a substitution.
throws-like 'my $x = "a"; $x ~~ s:overlap/a/b/', X::Syntax::Regex::Adverb,
    adverb => 'overlap', construct => 'substitution';
throws-like 'my $x = "a"; $x ~~ s:exhaustive/a/b/', X::Syntax::Regex::Adverb,
    adverb => 'exhaustive', construct => 'substitution';

# Compile-time switch adverbs are still fine on rx//.
ok 'abc' ~~ rx:i/ABC/, ':i is allowed on rx//';

# :g is allowed on m//.
ok ('a1b2' ~~ m:g/\d/).elems == 2, ':g is allowed on m//';
