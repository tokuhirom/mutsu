use v6;
use lib $?FILE.IO.parent.add('lib').Str;
use Test;

# Inside a module, a user-declared type whose short name collides with a
# built-in (e.g. `grammar Grammar`, `class Int`) must resolve, when referenced
# by its unqualified bareword name from a sub in that module, to the
# module-local declaration — not the built-in. Regression pin for the YAMLish
# battery: `unit module YAMLish` declares `grammar Grammar` and calls
# `Grammar.parse($input)` unqualified.

plan 5;

use ModuleLocalShadow;

# grammar Grammar (shadows core Grammar) must dispatch .parse
my $m = parse-it("12-34");
ok $m.defined, 'module-local grammar Grammar.parse dispatched (not core Grammar)';
is ~$m, "12-34", 'module-local grammar matched the input';

# its name resolves module-qualified, and .parse working proves GrammarHOW-ish behavior
is grammar-name(), 'ModuleLocalShadow::Grammar', 'bareword Grammar names the module-local grammar';

# class Int (shadows core Int) must resolve to the module-local class
my $i = make-int();
is $i.tag, 'module-local-Int', 'module-local class Int resolves over the built-in Int';

# sanity: the core Int is unaffected at the mainline (no shadowing declaration here)
is (40 + 2).WHAT.^name, 'Int', 'core Int still works where nothing shadows it';
