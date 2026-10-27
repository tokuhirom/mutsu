use Test;
use lib $*PROGRAM.parent(2).add('lib-prefix-class-parent');

# Constructs a `use`d module (Cro::HTTP::Client, YAMLish, Telemetry,
# CSS::Module) needs the RakuAST round trip to carry, found by running the
# whole t/ suite with the modules round-tripped (ADR-10723 section 6.126).
# Every program runs at the `convert` level, where no subtree crosses as the
# parser's own tree, and prints what raku prints.

plan 8;

sub run-convert(Str $code) {
    my %env = %*ENV;
    %env<MUTSU_RAKUAST> = 'convert';
    my $proc = run $*EXECUTABLE, '-I', $*PROGRAM.parent(2).add('lib-prefix-class-parent').Str,
        '-e', $code, :%env, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

{
    my ($rc, $out, $err) = run-convert('role R[$x] { method g { $x<a> } }; class C does R[%(a => 1, b => 2)] { }; say C.new.g');
    is $out, "1\n", 'a %(...) argument of a role application round-trips'
        or diag $err;
}

{
    my ($rc, $out, $err) = run-convert('grammar G { token TOP { $<sign=[+-]>? "a" } }; say so G.parse("a")');
    is $out, "True\n", 'a $<a=b> back-reference is a regex term with the key "a=b"'
        or diag $err;
}

{
    my $code = q:to/CODE/;
        my %h = a => 1;
        say qq:to/E/;
        x{ " y" if %h<a> }z
        E
        CODE
    my ($rc, $out, $err) = run-convert($code);
    is $out, "x yz\n\n", 'a statement modifier in a heredoc closure round-trips'
        or diag $err;
}

{
    my ($rc, $out, $err) = run-convert(
        'grammar H { use Pfx::Sub::Gram; also is Pfx::Sub::Gram; }; say H.^mro.map(*.^name).head(2).join(" ")');
    is $out, "H Pfx::Sub::Gram\n", 'also is Base in a grammar body is resolved after the body use'
        or diag $err;
}

{
    my ($rc, $out, $err) = run-convert(
        'grammar H { use Pfx::Sub::Gram; also is Pfx::Sub::Gram; }; say H.parse("abc", :rule<word>).so');
    is $out, "True\n", 'and the inherited token is callable'
        or diag $err;
}

{
    my ($rc, $out, $err) = run-convert('role Pfx::D { }; class Pfx::R does Pfx::D { }; say Pfx::R.new ~~ Pfx::D');
    is $out, "True\n", 'a type whose name ends in ::D is not the smiley :D'
        or diag $err;
}

{
    # An imported name makes a bareword an `ImportedTerm` in a unit that keeps
    # spellings; `require`'s operand is still a module name (#15074).
    my $code = q:to/CODE/;
        use JSON::Fast;
        require Pfx;
        say Pfx.hi;
        CODE
    my ($rc, $out, $err) = run-convert($code);
    is $out, "hi\n", 'require Module after a use that imports a name'
        or diag $err;
}

{
    # A module is `unit class Name:ver<..>:auth<..>;` with the whole rest of the
    # file as its body; the header adverbs are a typed node around the
    # declaration, which still absorbs the rest of the unit.
    my $code = q:to/CODE/;
        unit class Foo:ver<1.2>:auth<me>;
        method hi { "hi" }
        say Foo.^ver, Foo.^auth, Foo.hi;
        CODE
    my ($rc, $out, $err) = run-convert($code);
    is $out, "v1.2mehi\n", 'a unit class with header adverbs absorbs the rest of the unit'
        or diag $err;
}
