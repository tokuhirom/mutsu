use Test;

# A top-level frugal (lazy) quantifier — `.*?`, `.+?`, `X ** N..*?` — must match
# minimally under `:g`. mutsu's global-match path collected every possible match
# end at each start and then kept the LONGEST, which forced a frugal top-level
# quantifier to its greedy length: `"aXbXcX".match(/ .*? 'X' /, :g)` returned one
# match ("aXbXcX") instead of three ("aX","bX","cX"). Single (non-`:g`) matches
# were already correct, as were `:overlap` / `:exhaustive` and `s:g///`.

plan 10;

# --- .*? under :g ---
is "aXbXcX".match(/ .*? 'X' /, :g).map(*.Str).join('|'), 'aX|bX|cX',
    'frugal .*? matches minimally under :g';
is "aXbXcX".match(/ .*? 'X' /, :g).elems, 3, 'frugal .*? :g count';

# --- capturing frugal group under :g ---
is "aXbXcX".match(/ $<c>=(.*?) 'X' /, :g).map({ ~$_<c> }).join('|'), 'a|b|c',
    'captured frugal group is minimal under :g';

# --- .+? under :g ---
is "aXXbXX".match(/ 'a'? $<c>=(.+?) 'X' /, :g).map({ ~$_<c> }).join('|'), 'X|b',
    'frugal .+? matches minimally under :g';

# --- greedy .* is unchanged under :g (regression guard) ---
is "aXbXcX".match(/ .* 'X' /, :g).map(*.Str).join('|'), 'aXbXcX',
    'greedy .* still maximal under :g';

# --- single (non-:g) frugal was already correct ---
is ("aXbXcX" ~~ / $<c>=(.*?) 'X' /) && ~$<c>, 'a', 'frugal single match still minimal';

# --- frugal with a following anchor / literal fence body ---
{
    my $md = "```\nAAA\n```\n```\nBBB\n```\n";
    my @m = $md.match(/ '```' \v $<code>=(.*?) <?after \v> '```' /, :g);
    is @m.elems, 2, 'frugal fence body: two code blocks under :g';
    is ~@m[0]<code>, "AAA\n", 'first fence body is minimal';
    is ~@m[1]<code>, "BBB\n", 'second fence body is minimal';
}

# --- :exhaustive still returns every end (regression guard) ---
is "aaa".match(/ a+ /, :ex).elems, 6, ':exhaustive still enumerates all ends';
