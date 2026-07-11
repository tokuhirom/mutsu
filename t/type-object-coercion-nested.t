use v6;
use Test;

# Regression tests for three bugs surfaced while driving the real Zef CLI
# under mutsu (Zef::Utils::URI parsing path):
#   1. `run(Seq)` must flatten an iterable command list like Raku's slurpy args.
#   2. `Int.Int` / `Num.Num` / `Complex.Complex` on a *type object* return the
#      type object (identity coercion), not a "No such method" error.
#   3. A scalar typed with a nested `::` type name (`IO::Path`) defaults to that
#      exact type object, not its outer namespace (`IO`).

plan 13;

# --- 1. run() flattens a Seq/List first argument -----------------------------
{
    my @cmd = 'true',;
    my $seq = (|@cmd).grep(*.?chars);
    my $proc = run($seq, :!out, :!err);
    is $proc.exitcode, 0, 'run(Seq) flattens the command list and runs it';

    my $proc2 = run(('true',).Seq, :!out, :!err);
    is $proc2.exitcode, 0, 'run(Seq literal) flattens too';
}

# --- 2. identity coercion on numeric type objects ----------------------------
is Int.Int.^name, 'Int', 'Int.Int returns the Int type object';
nok Int.Int.defined, 'Int.Int is undefined (a type object)';
is Num.Num.^name, 'Num', 'Num.Num returns the Num type object';
is Complex.Complex.^name, 'Complex', 'Complex.Complex returns the Complex type object';

# The exact value that broke Zef's URI parser: `($missing // Int).Int`.
{
    my $port = (Any // Int).Int;
    is $port.^name, 'Int', '(Any // Int).Int yields the Int type object';
}

# --- 3. nested-`::` type name default type object ----------------------------
{
    my IO::Path $x;
    is $x.^name, 'IO::Path', 'uninitialized IO::Path scalar defaults to IO::Path';
}
{
    my IO::Path $x = Nil;
    is $x.^name, 'IO::Path', 'IO::Path scalar reset by Nil defaults to IO::Path';
}
{
    my IO::Path $x = '/tmp'.IO;
    is $x.^name, 'IO::Path', 'IO::Path scalar accepts an IO::Path value';
}
# A smiley on a nested type name must still be stripped for the default.
{
    my IO::Path:D $x = '/tmp'.IO;
    is $x.^name, 'IO::Path', 'IO::Path:D scalar keeps the nested type name';
}
# Optional parameter of a nested type defaults to the nested type object.
{
    sub f(IO::Path $p?) { $p.^name }
    is f(), 'IO::Path', 'optional IO::Path param defaults to IO::Path';
}
# Plain smiley stripping unaffected.
{
    sub g(Int:D $n) { $n }
    is g(5), 5, 'Int:D param still binds a plain Int';
}
