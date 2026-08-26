use v6;
use Test;

# `Code.line` / `Code.file` for a `Regex` code object: a grammar `token`/
# `rule`, a top-level `my token`/`my regex`, a token declared in a composed
# role, and a token inherited from a parent grammar.
#
# Follow-up to t/code-line-file-reflection.t, which covers Sub/Method/
# Submethod/Block/multi candidates; this file covers the one `Code` subtype
# that was left answering `Nil`: `Regex`.
#
# Every assertion is written against *relative* facts (line deltas,
# orderings, basename) so the file can gain a header or move without
# breaking. The file must pass under both `raku` and `mutsu`.

plan 14;

# ------------------------------------------------------------ grammar body

my $grammar-anchor = $?LINE;
grammar Digits {
    token digit { \d+ }
    rule spaced-digit { \d+ }
}

is Digits.^lookup("digit").line, $grammar-anchor + 2,
        'a grammar token .line is its declarator line';
is Digits.^lookup("spaced-digit").line - Digits.^lookup("digit").line, 1,
        'a token and a rule one line apart differ by one';
ok Digits.^lookup("digit").file.IO.basename eq 'code-line-file-regex-token.t',
        'a grammar token .file names this test file';
is Digits.^lookup("digit").file, Digits.^lookup("spaced-digit").file,
        'a token and a rule in the same grammar report the same .file';
is Digits.^lookup("digit").^name, 'Regex',
        '.^lookup on a token/rule reports the Regex type, not Method';

# ------------------------------------------------------- top-level declarations

my $top-anchor = $?LINE;
my token top-tok { \d+ }
my regex top-rgx { \d+ }

is &top-tok.line, $top-anchor + 1,
        'a top-level `my token` reports its own declarator line';
is &top-rgx.line, $top-anchor + 2,
        'a top-level `my regex` reports its own declarator line';
is &top-tok.file, Digits.^lookup("digit").file,
        'a top-level token reports the declaring file';

# ------------------------------------------------------------- role composition

my $role-anchor = $?LINE;
role Numeric {
    token numeric-tok { \d+ }
}
grammar WithRole does Numeric { }

is WithRole.^lookup("numeric-tok").line, $role-anchor + 2,
        'a token declared in a composed role keeps the role declaration line';
is WithRole.^lookup("numeric-tok").file, &top-tok.file,
        'a role-composed token reports the declaring file';

# ---------------------------------------------------------- grammar inheritance

my $parent-anchor = $?LINE;
grammar ParentG {
    token parent-tok { \d+ }
}
grammar ChildG is ParentG { }

is ChildG.^lookup("parent-tok").line, $parent-anchor + 2,
        'an inherited token .^lookup reports the parent declaration line';
is ChildG.^lookup("parent-tok").line, ParentG.^lookup("parent-tok").line,
        'an inherited token reports the same line as the declaring grammar';
is ChildG.^lookup("parent-tok").file, ParentG.^lookup("parent-tok").file,
        'an inherited token reports the same file as the declaring grammar';

# -------------------------------------------------------- the .^can contract

ok Digits.^lookup("digit").^can('line'), 'a Regex code object .^can("line")';
