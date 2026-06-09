use Test;

# X::Syntax::Regex::NullRegex — Raku rejects a regex (or one of its
# alternation/conjunction branches, or a group body) that matches nothing
# syntactically. mutsu detects these at parse/compile time.

plan 22;

# Whole-pattern empties.
throws-like q[/ /],     X::Syntax::Regex::NullRegex, 'bare empty regex';
throws-like q[s//b/],   X::Syntax::Regex::NullRegex, 'empty substitution pattern';

# Alternation branches.
throws-like q[/ a | /],  X::Syntax::Regex::NullRegex, 'trailing | branch';
throws-like q[/ a || /], X::Syntax::Regex::NullRegex, 'trailing || branch';
throws-like q[/ | /],    X::Syntax::Regex::NullRegex, 'two empty | branches';
throws-like q[/ || /],   X::Syntax::Regex::NullRegex, 'two empty || branches';
throws-like q[/ a | | b /], X::Syntax::Regex::NullRegex, 'interior empty | branch';

# Conjunction branches.
throws-like q[/ a & /],  X::Syntax::Regex::NullRegex, 'trailing & branch';
throws-like q[/ a && /], X::Syntax::Regex::NullRegex, 'trailing && branch';

# Groups.
throws-like q{/ () /},   X::Syntax::Regex::NullRegex, 'empty capture group';
throws-like q{/ [] /},   X::Syntax::Regex::NullRegex, 'empty non-capturing group';
throws-like q{/ ( ) /},  X::Syntax::Regex::NullRegex, 'whitespace-only capture group';
throws-like q{/ (a|) /}, X::Syntax::Regex::NullRegex, 'trailing empty branch in group';

# Grammar / named regex bodies.
throws-like q[my grammar G { regex foo { } }], X::Syntax::Regex::NullRegex, 'empty grammar regex';
throws-like q[my grammar G { token foo { } }], X::Syntax::Regex::NullRegex, 'empty grammar token';
throws-like q[my grammar G { rule foo { } }],  X::Syntax::Regex::NullRegex, 'empty grammar rule';
throws-like q[my regex r { }], X::Syntax::Regex::NullRegex, 'empty my regex';

# These are NOT null and must keep working.
lives-ok { EVAL q[/ ^ /] },     'anchor-only regex is fine';
lives-ok { EVAL q[/ "" /] },    'empty string literal atom is fine';
lives-ok { EVAL q[/ <?> /] },   'zero-width assertion is fine';
lives-ok { EVAL q[/ | a /] },   'leading empty | branch (alignment) is fine';
lives-ok { EVAL q[/ (|a) /] },  'leading empty branch in group is fine';
