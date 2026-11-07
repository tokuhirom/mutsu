use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 3;

# A tree evaluated with EVAL($ast) lists its Pod blocks and declarator blocks in
# the order the tree has them, as the source's own $=pod would.
my $src = Q[#| lead
sub foo() { }
=begin pod
hello
=end pod
#| second
sub bar() { }
$=pod.map(*.^name.split('::').tail).join(" ")
];
is EVAL($src.AST), 'Declarator Named Declarator', 'EVAL($ast): declarator, Pod, declarator';
is EVAL($src), 'Declarator Named Declarator', 'EVAL of the source gives the same order';

my $only = Q[=begin pod
first
=end pod
#| doc
sub baz() { }
$=pod.map(*.^name.split('::').tail).join(" ")
];
is EVAL($only.AST), 'Named Declarator', 'a Pod block before the declarator stays first';
