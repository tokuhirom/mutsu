use Test;

# Found via Graph::RandomMaze (Math::Nearest): a candidate with a required
# `@` positional must not match a call that supplies too few positionals.
plan 4;

proto nr(|) {*}
multi sub nr(@p, :$method = Whatever) { "A" }
multi sub nr(@p, $s where * !~~ (Iterable:D), **@args, *%args) { "B" }
multi sub nr(@p, @s, UInt $c = 1, :$prop = Whatever, *%a) { "C" }
multi sub nr(@p, @s, ($count, $radius), :$prop = Whatever, *%a) { "D" }

my @pts = [1, 2], [3, 4];
is nr(@pts, method => 'K'), 'A', 'named-only call skips candidates needing a second positional';
is nr(@pts, 5), 'B', 'scalar search point';
is nr(@pts, [1, 2], 3), 'C', 'array + count';
is nr(@pts, [1, 2], (3, 4)), 'D', 'array + sub-signature';
