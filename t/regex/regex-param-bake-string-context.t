use Test;

plan 16;

# A `token`/`rule`/`regex` parameter reaches the pattern's code blocks by
# textual substitution, and the argument is rendered back into the pattern as
# the subrule call's argument. Both splices land inside a *double-quoted* Raku
# string, so every character that could start an interpolation has to be
# escaped — otherwise the spliced text is read back under the enclosing
# string's own rules and the value silently becomes something else.
# https://github.com/tokuhirom/mutsu/issues/8317
#
# `$b`, `@b` and `%b` exist here on purpose: if a spliced `$b` is ever
# re-interpolated it resolves to one of these instead of staying literal, which
# is precisely the failure this pins.
my $b = 'ZZZ';
my @b = <ZZZ>;
my %b = :ZZZ;

my @log;

# Each token logs its parameter twice: once read as a bare term (a code
# position) and once through a `"..."` string (an interpolation position).
sub check($grammar, Str $expect, Str $what) {
    @log = ();
    $grammar.parse('zz');
    is @log[0], $expect,        "$what: code position";
    is @log[1], "[$expect]",    "$what: inside a double-quoted string";
}

grammar Dollar    { token TOP { <t('a$b')>   }; token t($x) { { @log.push($x, "[$x]") } \w+ } }
grammar At        { token TOP { <t('a@b')>   }; token t($x) { { @log.push($x, "[$x]") } \w+ } }
grammar Percent   { token TOP { <t('a%b')>   }; token t($x) { { @log.push($x, "[$x]") } \w+ } }
grammar Amp       { token TOP { <t('a&b')>   }; token t($x) { { @log.push($x, "[$x]") } \w+ } }
grammar Brace     { token TOP { <t('a{b}c')> }; token t($x) { { @log.push($x, "[$x]") } \w+ } }
grammar Quote     { token TOP { <t('a"b')>   }; token t($x) { { @log.push($x, "[$x]") } \w+ } }
grammar Backslash { token TOP { <t('a\\b')>  }; token t($x) { { @log.push($x, "[$x]") } \w+ } }

check Dollar,    'a$b',   'a `$` in the argument';
check At,        'a@b',   'a `@` in the argument';
check Percent,   'a%b',   'a `%` in the argument';
check Amp,       'a&b',   'a `&` in the argument';
check Brace,     'a{b}c', 'a `{ }` in the argument';
check Quote,     'a"b',   'a `"` in the argument';
check Backslash, 'a\\b',  'a backslash in the argument';

# The positions that were already right must stay right: a `{ ... }` nested in
# a string is a *code* position again, and a single-quoted string does not
# interpolate at all.
grammar Nested { token TOP { <t('bar')> }; token t($x) { { @log.push("[{$x}]", 'lit=$x') } \w+ } }
@log = ();
Nested.parse('zz');
is @log[0], '[bar]', 'a code block nested in a string still substitutes';
is @log[1], 'lit=' ~ '$' ~ 'x', 'a single-quoted string does not interpolate the parameter';
