use v6;
# NOTE: a string-literal `use lib` path is required here (not an expression like
# `$?FILE.IO.parent.add('lib')`): the parser scans `use`d module files at parse
# time to learn their declared types, and only a literal path updates the
# parse-time library search path. `prove` runs from the repo root, so `t/lib`
# resolves.
use lib 't/lib';
use Test;
use ImportedExceptionType;

# An X::/CX:: exception type imported from a used module must be recognized as a
# declared type by the parser, so `when X::Imported::Boom { ... }` is NOT flagged
# as an undeclared bareword gobbling its block. Regression: the parser only saw
# types declared in the current compilation unit, so a `when` matching an imported
# exception type raised X::Comp::Group("Missing block") at parse time. This broke
# real code such as Zef's `when X::Zef::UnsatisfiableDependency { ... }` in a file
# that `use Zef`.

plan 4;

# Plain given/when on the imported exception type.
{
    my $r = "no";
    given X::Imported::Boom.new {
        when X::Imported::Boom { $r = "matched" }
        default { $r = "default" }
    }
    is $r, "matched", 'imported X:: exception type matches in given/when';
}

# The imported type is a valid smartmatch target outside given as well.
ok X::Imported::Boom.new ~~ X::Imported::Boom, 'imported exception smartmatches its own type';

# The exact zef shape: a `when` on the imported type nested inside a CATCH inside
# a block that is the condition of an `if` (this is what regressed to a parse
# error before the fix).
{
    my @vals = 1, 2, 3;
    my $branch = "none";
    if @vals.first({
            CATCH { when X::Imported::Boom { } }
            $_ > 2
        }) -> $ {
        $branch = "then";
    } else {
        $branch = "else";
    }
    is $branch, "then", 'when-on-imported-type inside if-condition block parses and runs';
}

# The undeclared case must still error: a genuinely-unknown X:: bareword gobbles.
throws-like 'when X::TotallyUndeclared::Nope {}', X::Comp::Group,
    'genuinely undeclared X:: bareword in when still raises the gobble error';
