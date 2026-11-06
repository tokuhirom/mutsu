use Test;
plan 2;

try { (1..10).Seq.skip(2, *); CATCH { default {
    like .message.lines[0], /'skip(Seq:D: Int:D, Whatever:D)'/, 'a Whatever argument is named Whatever:D';
} } }

try { (1, 2, 3).Seq.skip(2, **); CATCH { default {
    like .message.lines[0], /'Int:D, HyperWhatever:D'/, 'and a HyperWhatever one HyperWhatever:D';
} } }
