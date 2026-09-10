use lib $*PROGRAM.parent(1).add("lib");
use lib $*PROGRAM.parent(1).add("..").add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# An exported MAIN candidate declared inside a `package` block must see a `my`
# lexical assigned in the package-block mainline, when the program is run via
# `use Module;` + MAIN auto-dispatch. This is the zef `Zef::CLI` pattern:
#   package Zef::CLI { my $CONFIG = ...; proto MAIN(|) is export {*};
#                      multi MAIN('info', ...) { ... $CONFIG ... } }
# (Uses get_out + is rather than is_run: is_run's result-comparison smartmatch is
# unrelated to this feature.)

plan 4;

{
    my %got = get_out(
        'use PackageMainLexical;', '',
        :args['show'], :compiler-args['-I', 't/lib'],
    );
    is %got<out>, "config=from-mainline\n",
        'package-block MAIN reads a mainline `my` lexical (via use + MAIN dispatch)';
}

{
    my %got = get_out(
        'use PackageMainLexical;', '',
        :args['twice'], :compiler-args['-I', 't/lib'],
    );
    is %got<out>, "config=from-mainlinefrom-mainline\n",
        'a second MAIN candidate sees the same mainline lexical';
}

# A top-level `my` mutated AFTER a sub's declaration is read at its current
# value when the sub runs (closures share the live container, not a snapshot).
{
    my $x = "before";
    sub read-x() { $x }
    $x = "after";
    is read-x(), "after", 'sub reads the live value of a captured outer `my`';
}

# Same, with accumulation through repeated mutation.
{
    my $n = 0;
    sub bump() { $n++ }
    bump(); bump(); bump();
    is $n, 3, 'sub mutating a captured outer `my` accumulates';
}
