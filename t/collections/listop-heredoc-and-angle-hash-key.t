use v6;
use Test;

# Two parser fixes surfaced by App::Rak:
#  1. A forward-referenced (declared-later) sub called listop-style with a
#     heredoc argument (`meh q:to/END/ ...`) must parse — the undeclared-ident
#     listop gate did not recognize a `q`/`Q` quote construct as a term start.
#  2. `#` is an ordinary character inside angle-word quoting, so a hash angle
#     subscript key may contain it (`%exts<#csv>`).

plan 8;

# --- 1. forward-referenced listop with a heredoc argument ---
{
    my @got;
    collect q:to/END/.chomp;
    line one
    line two
    END
    is @got[0], "line one\nline two", 'forward listop takes a heredoc arg (with .chomp)';

    my sub collect($text) { @got.push: $text }
}
{
    my @got;
    collect2 q:to/END/;
    body text
    END
    is @got[0], "body text\n", 'forward listop takes a bare heredoc arg';
    my sub collect2($text) { @got.push: $text }
}
{
    my @got;
    collect3 qw<a b c>;
    is @got[0], <a b c>, 'forward listop takes a qw<> word-list arg';
    my sub collect3(@words) { @got.push: @words }
}

# --- 2. `#` inside an angle subscript key ---
{
    my %h = "#csv" => 42, "csv" => 7;
    is %h<#csv>, 42, 'angle subscript key may contain #';
    is %h<csv>, 7, 'adjacent plain key unaffected';
}
{
    my %h = "#a-b" => "x";
    is %h<#a-b>, "x", 'angle key with # and a hyphen';
}

# Regression: a real trailing comment still works after a subscript, and the
# `<...>` word-list literal with `#` still produces the literal word.
{
    my %h = a => 1;
    is %h<a>, 1, 'plain subscript then # comment parses'; # trailing comment
    is <#csv>, ("#csv",), '<#csv> word list is the literal word';
}
