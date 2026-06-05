use Test;

plan 13;

# IO::Path .CWD / .SPEC methods
{
    my $p = IO::Path::Unix.new("/foo|\\bar", :CWD("/tmp/here"));
    is $p.CWD, "/tmp/here", '.CWD returns the stored CWD';
    is $p.SPEC.^name, "IO::Spec::Unix", '.SPEC returns the IO::Spec type';
    # .raku.EVAL roundtrip preserves CWD and SPEC (eqv)
    ok $p.raku.EVAL.CWD  eqv $p.CWD,  '.raku.EVAL.CWD roundtrips';
    ok $p.raku.EVAL.SPEC eqv $p.SPEC, '.raku.EVAL.SPEC roundtrips';
}

# .SPEC.dir-sep works on the returned type object
is IO::Path::Unix.new("foo").SPEC.dir-sep, '/', '.SPEC.dir-sep works';

# Named sub-signature destructuring in for-loops
{
    # Hash: bind by key
    my @res;
    for (%(orig => 'a', with => 'b'),) -> (:$orig, :$with) {
        @res.push: "$orig-$with";
    }
    is @res, ['a-b'], 'for-loop named unpack of Hash binds by key';
}

{
    # Pair: bind via .key/.value accessors (Pair is Associative but has methods)
    my $tracker = '';
    for a => 1, b => 2 -> (:$key, :$value) {
        $tracker ~= "|$key,$value";
    }
    is $tracker, '|a,1|b,2', 'for-loop named unpack of Pair binds key/value';
}

{
    # Object: bind via public attribute accessor
    class A { has $.x }
    my @res;
    for A.new(x => 4), A.new(x => 2) -> (:$x) {
        @res.push: $x;
    }
    is @res, [4, 2], 'for-loop named unpack of object binds attribute reader';
}

# words() with a $limit argument
{
    my $file = "tmp/words-limit-test-{$*PID}.txt".IO;
    LEAVE $file.unlink;
    $file.spurt: "a b c d e f";

    is words($file.open, 2).elems, 2, 'words($fh, 2) sub form honors limit';
    is $file.open.words(3).List, <a b c>, '$fh.words(3) method form honors limit';
    is $file.words(2).List, <a b>, 'IO::Path.words(2) honors limit';

    # :close closes the handle when the iterator is exhausted
    my $fh = $file.open;
    my @all = words($fh, :close);
    is @all.elems, 6, 'words($fh, :close) reads all words';
    is-deeply $fh.opened, False, 'words($fh, :close) closes the handle';
}
