use Test;

# A trailing `#=` documents a parameter only right after its variable
# (`$a #= doc`, `Int $a? #= doc`) or after the `,` that ends it. After a
# default, a `where` or a trait with no `,` before the comment, and after a
# sigilless `\a`, it documents the routine instead -- as in Rakudo.
# mutsu#10953.

plan 20;

sub a1($a = 1 #= doc
) {}
is &a1.WHY, 'doc', 'after a default: the routine';
is-deeply &a1.signature.params.map(*.WHY.Str), ('',), 'and not the parameter';

sub a2($a = 1, #= doc
) {}
nok &a2.WHY.defined, 'after the default and a comma: not the routine';
is &a2.signature.params[0].WHY, 'doc', 'but the parameter';

sub a3(Int $a where * > 0 #= doc
) {}
is &a3.WHY, 'doc', 'after a where clause: the routine';

sub a4($a is copy #= doc
) {}
is &a4.WHY, 'doc', 'after a trait: the routine';

sub a5(:$a = 1 #= doc
, :$b) {}
is &a5.WHY, 'doc', 'a comma after the comment does not count';
nok &a5.signature.params[0].WHY.defined, 'so the parameter has none';

sub a6($a, $b = 2 #= doc
) {}
is &a6.WHY, 'doc', 'the last of several parameters, defaulted';

sub a7(\a #= doc
) {}
is &a7.WHY, 'doc', 'after a sigilless parameter: the routine';
nok &a7.signature.params[0].WHY.defined, 'never the sigilless parameter';

sub a8(\a, #= doc
) {}
is &a8.WHY, 'doc', 'even after its comma';

sub b1($a #= doc
) {}
nok &b1.WHY.defined, 'right after the variable: not the routine';
is &b1.signature.params[0].WHY, 'doc', 'but the parameter';

sub b2(Int $a? #= doc
) {}
is &b2.signature.params[0].WHY, 'doc', 'after a type and `?`';

sub b3(:$a! #= doc
) {}
is &b3.signature.params[0].WHY, 'doc', 'after a named `!`';

sub b4($a #= doc
, $b) {}
is &b4.signature.params[0].WHY, 'doc', 'before the comma of a plain parameter';

sub b5($a where * > 0, #= doc
) {}
is &b5.signature.params[0].WHY, 'doc', 'a where clause and a comma';

sub b6($a = "x # y" #= doc
) {}
is &b6.WHY, 'doc', 'a `#` inside a string default is not a comment';

sub b7(#| lead
$a = 1) {}
is &b7.signature.params[0].WHY, 'lead', 'a leading doc still documents a defaulted parameter';
