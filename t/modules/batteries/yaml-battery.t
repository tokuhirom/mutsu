use Test;

# YAML is bundled (BATTERIES.md §7, docs/batteries/yaml.md), so `YAMLish` must
# load and work with a plain `use` -- no `-I`, no install. This pins the
# zero-config resolution and a smoke slice of the API; the exhaustive behaviour
# check is the release-time gate that runs the full upstream suite
# (scripts/battery-testsuite.sh).

plan 9;

use YAMLish;

is-deeply load-yaml("- 1\n- 2\n"), [1, 2], 'load-yaml reads a block sequence';
is-deeply load-yaml("a: 1\nb: two\n"), { a => 1, b => 'two' },
    'load-yaml reads a block mapping';
is-deeply load-yaml("[1, 2, three]\n"), [1, 2, 'three'],
    'load-yaml reads a flow sequence';
is load-yaml("--- |\n  one\n  two\n...\n"), "one\ntwo\n",
    'load-yaml reads a literal block scalar';
is-deeply load-yaml("- &a 1\n- *a\n"), [1, 1], 'load-yaml resolves an alias';

is load-yamls("---\na: 1\n---\na: 2\n").elems, 2, 'load-yamls reads a stream';

is save-yaml({ b => 2, a => 1 }), qq{---\n"a": 1\n"b": 2\n...},
    'save-yaml emits a mapping with sorted keys';
is save-yaml('a string'), qq{--- "a string"\n...}, 'save-yaml quotes a string';

is-deeply load-yaml(save-yaml({ x => [1, 2], y => 'z' })), { x => [1, 2], y => 'z' },
    'save-yaml and load-yaml round-trip';
