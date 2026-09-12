use Test;

# The dotted spelling of a postcircumfix subscript (`.[...]`, `.{...}`,
# `.<...>`, `.<<...>>`, `.«...»`) is the same subscript as the undotted one.
# mutsu used to restate a weaker copy of each after a `.`, so semicolon
# dimensions, the zen slice `.<>`, nested-angle keys and the interpolating
# angle spellings were all silently missing after a dot.
# From Game::Entities 0.1.6 (`.[COMPONENTS; $i].<>`), which could not load.

plan 14;

my %h = a => 1, b => 2;
my @a = [1, 2];
my $c = [[1, 2], [3, 4]];
my %n = a => { b => 1 };

# `.<key>` and `.<a b>` (already worked; pinned so the delegation keeps them)
is %h.<a>, 1, '.<key> looks one key up';
is-deeply %h.<a b>, (1, 2), '.<a b> slices';
is %h.<a>:exists, True, '.<key>:exists';

# The zen slice `.<>`
is-deeply %h.<>, %h, '.<> is the whole hash';
is-deeply %h.<>:k.sort, ('a', 'b'), '.<>:k gives the keys';
is-deeply @a.<>, [1, 2], '.<> on an array';
is-deeply $(1, 2).<>, (1, 2), '.<> decontainerizes an itemized list';

# Nested angle brackets inside the key
my %nested = 'a<b>' => 7;
is %nested.<a<b>>, 7, '.<a<b>> is one literal key';

# The interpolating spellings
my $k = 'a';
is %h.<<$k>>, 1, '.<<$k>> interpolates';
is %h.«$k», 1, '.«$k» interpolates';

# Semicolon dimensions
is $c.[0; 1], 2, '.[0; 1] indexes two dimensions';
# Bound first: a multi-dim subscript passed straight to a listop flattens
# (mutsu bug, and it predates this fix — the undotted spelling does it too).
my $dotted = %n.{'a'; 'b'};
is-deeply $dotted, (1,), '.{...;...} indexes two dimensions';

# Zen `.[]` / `.{}` still select the whole container
is-deeply @a.[], [1, 2], '.[] is the whole array';
is-deeply %h.{}, %h, '.{} is the whole hash';
