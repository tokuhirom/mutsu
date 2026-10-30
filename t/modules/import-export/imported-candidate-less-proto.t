use Test;

# A proto with no candidates whose body answers the call itself is an
# ordinary routine (#10696), and an import of one is the import of that
# routine (#13994): a top-level `use` makes it callable, and a block-scoped
# `use` binds it in the block, where it wins over the importer's routine of
# the same name, a closure keeps it, and a callee written outside the block
# keeps seeing the importer's routine. An imported only-star proto does not
# hide the importer's routine from such a callee either.
plan 7;

use lib 't/lib';

sub ibp-answer($x) { "main $x" }
sub ibp-star($x) { "main star $x" }
sub ibp-callee { ibp-answer(1) ~ ' / ' ~ ibp-star(1) }

{
    use ImportBodiedProto;
    is ibp-answer(1), 'IBP answer 1', 'inside the block the imported proto answers';
    is ibp-callee(), 'main 1 / main star 1',
        'a callee written outside the block sees the importer\'s routines';
}

my &keep = do { use ImportBodiedProto; -> $x { ibp-answer($x) } };
is keep(2), 'IBP answer 2', 'a closure taken in the block keeps the import';
is ibp-answer(3), 'main 3', 'after the block the importer\'s routine answers';

sub run-top(Str $code) {
    my $p = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    $p.out.slurp(:close).trim
}
is run-top('use ImportBodiedProto; print ibp-answer(4)'), 'IBP answer 4',
    'a top-level use of a unit module makes the proto callable';
is run-top('use ImportBodiedProtoBare; print ibpb-answer(5)'), 'IBPB answer 5',
    'so does a top-level use of a bare-file module';
my &bare = do { use ImportBodiedProtoBare; -> $x { ibpb-answer($x) } };
is (try bare(6)), 'IBPB answer 6', 'a closure keeps a bare-file module\'s proto';
