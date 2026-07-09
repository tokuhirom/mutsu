use v6;
use Test;

# An itemized array (`$[...]`) is still an Array for introspection methods like
# .keys/.values/.kv/.pairs/.antipairs: they report the array's own elements and
# do NOT collapse the itemized array into a single opaque list element (which is
# the correct behavior only for list flattening). Regression for the Zef
# `Zef::Utils::SystemQuery::system-collapse` `for $data.keys` traversal, which
# received an itemized array and silently dropped every element past the first.

plan 25;

# --- itemized array, no hash element ---
my $a = $["A", "B", "C"];
is $a.elems, 3, 'itemized array elems';
is-deeply $a.keys.List, (0, 1, 2), 'itemized array keys';
is-deeply $a.values.List, ("A", "B", "C"), 'itemized array values';
is-deeply $a.kv.List, (0, "A", 1, "B", 2, "C"), 'itemized array kv';
is-deeply $a.pairs.List, (0 => "A", 1 => "B", 2 => "C"), 'itemized array pairs';
is-deeply $a.antipairs.List, ("A" => 0, "B" => 1, "C" => 2), 'itemized array antipairs';

# --- itemized array containing a Hash element (the Zef case) ---
my $b = $["Zef::Client", {:from("native"), :name("unknown")}];
is $b.elems, 2, 'itemized array with hash elems';
is-deeply $b.keys.List, (0, 1), 'itemized array with hash keys';
is $b.values.elems, 2, 'itemized array with hash values count';
is $b.values[0], "Zef::Client", 'itemized array with hash values[0]';
is $b.values[1]<name>, "unknown", 'itemized array with hash values[1] is the hash';
is $b.kv.elems, 4, 'itemized array with hash kv count';
is $b.pairs.elems, 2, 'itemized array with hash pairs count';
is $b.pairs[1].key, 1, 'itemized array with hash pairs[1] key';
is $b.pairs[1].value<name>, "unknown", 'itemized array with hash pairs[1] value';
is $b.antipairs.elems, 2, 'itemized array with hash antipairs count';

# --- via a scalar variable holding an array (implicitly itemized) ---
my $c = ["X", {:y(2)}];
is-deeply $c.keys.List, (0, 1), 'scalar-held array keys';
is $c.values.elems, 2, 'scalar-held array values count';

# --- for-loop over itemized array keys drives every index ---
my @seen;
for $b.keys -> $k { @seen.push($k) }
is-deeply @seen.List, (0, 1), 'for over itemized keys visits every index';

# --- plain (non-itemized) array unchanged ---
my @plain = ("A", "B", "C");
is-deeply @plain.keys.List, (0, 1, 2), 'plain array keys unchanged';
is-deeply @plain.values.List, ("A", "B", "C"), 'plain array values unchanged';
is-deeply @plain.kv.List, (0, "A", 1, "B", 2, "C"), 'plain array kv unchanged';
is-deeply @plain.pairs.List, (0 => "A", 1 => "B", 2 => "C"), 'plain array pairs unchanged';

# --- itemized array in flattening context stays a single element (the point of
#     itemization): .keys/.values descend, but flat/list context does not. ---
my @flat = flat "head", $["x", "y"];
is @flat.elems, 2, 'itemized array does NOT flatten in list context';
is-deeply @flat[1].List, ("x", "y"), 'itemized array preserved as one element';
