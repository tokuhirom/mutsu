use Test;

# A method call written at the start of a continuation line
# (`$x = "abc"\n    .uc.lc`) is an `ApplyDottyInfix(left, DottyInfix::Call,
# Call::Method)`. Its pieces carry the ranges rakudo gives them (#15780): the
# `Call` is the dot alone, the `Method` and its `Name` the name, and the
# `ArgList` the arguments (empty right after the name when there are none).

plan 4;

use experimental :rakuast;

sub origins(Str $src) {
    my @l;
    sub w($n) { @l.push($n.^name.split('::').tail ~ "@" ~ .from ~ "-" ~ .to) with $n.origin; $n.visit-children(&w) }
    w($src.AST);
    @l.grep(/^[Call | Method | Name | ArgList | ApplyDottyInfix]/).join(' ');
}

is origins("my \$q = 'abc'\n    .uc;"),
    'ApplyDottyInfix@8-21 Call@18-19 Method@19-21 Name@19-21 ArgList@21-21', 'one call on the next line';
is origins("my \$q = 'abc'\n    .uc.lc;"),
    'ApplyDottyInfix@8-24 ApplyDottyInfix@8-21 Call@18-19 Method@19-21 Name@19-21 ArgList@21-21 Call@21-22 Method@22-24 Name@22-24 ArgList@24-24',
    'a chain';
is origins("my \$q = 'abc'\n    .substr(1)  # comment\n    .uc;").contains('Call@4:'), False, 'a comment between the lines';
is origins("my \$q = 'abc'\n    .substr(1);"),
    'ApplyDottyInfix@8-28 Call@18-19 Method@19-28 Name@19-25 ArgList@26-27', 'with an argument';
