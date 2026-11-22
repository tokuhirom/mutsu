use Test;

# #18731: rakudo's `X::Undeclared.message` with no suggestion appends the
# forgotten-`sub` hint, word-wrapped at 72 columns.

plan 5;

my $hint = "Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?";

is X::Undeclared.new(symbol => '$x').message, "Variable '\$x' is not declared. $hint",
    'a $-sigiled symbol';
is X::Undeclared.new(symbol => '&f').message, "Variable '&f' is not declared. $hint",
    'a &-sigiled symbol';
is X::Undeclared.new(symbol => '$x', suggestions => <$y>).message,
    "Variable '\$x' is not declared. Did you mean '\$y'?", 'a suggestion replaces the hint';
is X::Undeclared.new(symbol => '$averyveryverylongvariablenamethatgoesonandonandonandonandonandon').message,
    "Variable\n'\$averyveryverylongvariablenamethatgoesonandonandonandonandonandon' is\nnot declared. Perhaps you forgot a 'sub' if this was intended to be part\nof a signature?",
    'a long symbol wraps';

try EVAL q[say $xyzq];
is $!.message, "Variable '\$xyzq' is not declared. Perhaps you forgot a 'sub' if this\nwas intended to be part of a signature?",
    'an undeclared variable in EVAL';
