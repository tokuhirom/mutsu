use Test;

# `done` inside a `supply` block ends the supply. A tree read back from source
# lowers `done` to the bare word the compiler resolves (a lexical `&done`
# shadows it), and the supply's expansion has to rewrite that onto the emitter
# as it does the parser's own `done` statement.

plan 5;

sub run($src) { EVAL($src.AST) }

is run(Q[my @got; my $s = supply { emit 1; emit 2; done; emit 3 }; $s.tap({ @got.push: $_ }); @got.join(',')]),
    '1,2', '`done` stops the supply body';

is run(Q[my $closed = False; my $s = supply { emit 1; done }; $s.tap(-> $v { }, done => { $closed = True }); $closed]),
    True, 'the done callback runs';

is run(Q[my @got; my $s = supply { emit $_ for 1..5; done if @got.elems >= 0; emit 9 }; $s.tap({ @got.push: $_ }); @got.join(',')]),
    '1,2,3,4,5', '`done` with a statement modifier';

is run(Q[my @got; react { whenever Supply.from-list(1, 2, 3) { @got.push: $_; done if $_ == 2 } }; @got.join(',')]),
    '1,2', '`done` ends a react block';

# A lexical &done is not the completion.
is run(Q[my &done = -> { 'mine' }; done()]), 'mine', 'a lexical &done shadows the keyword';
