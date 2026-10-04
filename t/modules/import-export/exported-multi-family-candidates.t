use lib $?FILE.IO.parent(3).add('lib');
use Test;

# Exporting a multi family aliases each candidate into the EXPORT stash as it
# is declared (#11761); every candidate must still be importable, whatever
# order the proto, the exported and the unexported candidates came in.

plan 5;

subtest 'proto exported before its candidates', {
    plan 5;
    use ExportedMultiFamily;
    is describe(1), 'Int 1', 'first candidate';
    is describe('a'), 'Str a', 'second candidate';
    is describe(1, 2), 'Int,Int 1 2', 'arity-2 candidate';
    is describe(1/2), 'Rat 0.5', 'later candidate';
    is describe(), 'nothing', 'last candidate';
}

subtest 'every candidate exported', {
    plan 4;
    use ExportedMultiFamily;
    is shape(1), 'int-shape 1', 'first candidate';
    is shape('x'), 'str-shape x', 'second candidate';
    is shape(1, 'y'), 'pair-shape 1 y', 'candidate with an extra tag';
    is shape(1e0), 'num-shape 1', 'candidate after the extra tag';
}

subtest 'candidates around the exported one', {
    plan 4;
    use ExportedMultiFamily;
    is late(1), 'late-int 1', 'candidate declared before the export';
    is late('s'), 'late-str s', 'second candidate declared before the export';
    is late(2e0), 'late-num 2', 'the exported candidate';
    is late(True), 'late-bool True', 'candidate declared after the export';
}

subtest 'a tag only one candidate named', {
    plan 1;
    use ExportedMultiFamily :extra;
    is shape(3, 'z'), 'pair-shape 3 z', ':extra imports the family';
}

subtest 'a tag only the first candidate carries', {
    plan 4;
    {
        use ExportedMultiFamily :early;
        is tagfirst(1), 'tagfirst-int 1', ':early imports the tagged candidate';
        is tagfirst('a'), 'tagfirst-str a', ':early imports the later candidate';
    }
    {
        use ExportedMultiFamily;
        is tagfirst(2), 'tagfirst-int 2', 'DEFAULT imports the tagged candidate';
        is tagfirst('b'), 'tagfirst-str b', 'DEFAULT imports the later candidate';
    }
}
