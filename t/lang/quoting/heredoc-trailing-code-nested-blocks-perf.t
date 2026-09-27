use Test;

plan 2;

# #9674: a heredoc whose marker line carries trailing code
# (`is Q:to[END], 'a', 'x';`) makes `parse_to_heredoc_with_flags`
# (src/parser/primary/string/heredoc.rs) splice that trailing code onto the
# text after the terminator and return the splice as its remainder. That
# remainder is not a subslice of the string it was parsed from, so
# `ParseMemo::store` (src/parser/memo.rs) used to refuse to cache it. Every
# backtracking attempt the statement/expression parsers made through an
# enclosing block then re-parsed -- and re-`Box::leak`ed -- the same heredoc
# from scratch, multiplying cost at every level of block nesting. Eight
# levels of `subtest 'n', { BODY BODY }`, each BODY holding one such heredoc,
# doubles the heredoc count per level (256 heredocs at depth 8) and used to
# make parsing take minutes and exhaust memory; a plain non-heredoc body of
# the same shape stayed fast throughout. The fix lets the memo record a
# remainder that lives in a permanently leaked buffer instead of refusing it
# (`primary::is_within_leaked_region`), so a retried parse hits the cache
# instead of re-leaking.

sub nested-heredoc-source(Int $depth) {
    my $body = "is Q:to[END], 'a', 'x';\na\nEND\n";
    my $s = $body;
    for ^$depth {
        $s = "subtest 'n', \{\n" ~ $s ~ $s ~ "\};\n";
    }
    $s
}

my $depth = 8;
my $file = $*TMPDIR.add("heredoc-nest-perf-{$*PID}-{(^10000).pick}.t");
$file.spurt(nested-heredoc-source($depth));
LEAVE { try $file.unlink }

my $out = '';
my $prog = Proc::Async.new($*EXECUTABLE.absolute, '--dump-ast', $file.absolute);
$prog.stdout.tap: { $out ~= $^a };
my $promise = $prog.start;
# A generous ceiling: the fixed parser dumps this AST in well under a second
# on a release build. Before the fix, depth 4 of the same shape already took
# 13+ seconds and depth 5 exhausted memory, so ten seconds is still a huge
# margin over the fix and nowhere near what the bug would have needed.
await Promise.anyof(Promise.in(10), $promise);
my $finished = $promise.status == Kept;
$prog.kill unless $finished;
ok $finished,
    'parsing depth-8 nested blocks of a trailing-code heredoc does not blow up exponentially';

# Every one of the 2**depth heredoc bodies survived intact -- confirms the
# memoized parse actually completed a correct, full-depth parse rather than
# bailing out early or losing content along the way.
my $expected-heredocs = 2 ** $depth;
is $out.comb(/ '"a\n",' /).elems, $expected-heredocs,
    'every heredoc body in the nested blocks parsed correctly';
