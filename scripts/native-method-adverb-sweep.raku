#!/usr/bin/env raku

# The measurement behind ADR-0070: how many builtin-method calls are NOT
# named-blind?
#
# Every Raku *method* carries an implicit `*%_`, so an adverb the method does
# not declare cannot change its answer. This script probes that property
# directly: for every (owner, method, argument-shape) triple derived from
# `src/builtins/native_method_row_table.rs` it evaluates
#
#     RECEIVER.method(ARGS)          and          RECEIVER.method(ARGS, :qqzz9)
#
# and reports every probe whose two answers differ. `:qqzz9` is a name no Raku
# routine declares, so under a conforming implementation the two must agree --
# either both give the same value or both die the same way.
#
# The script is implementation-agnostic: run it under the interpreter you want
# to measure.
#
#     raku                    scripts/native-method-adverb-sweep.raku   # oracle
#     ./target/release/mutsu  scripts/native-method-adverb-sweep.raku   # mutsu
#
# Pass `-v` (as the first argument) to list every diverging probe rather than
# only the count. The count is the campaign's progress metric; the residue is
# tracked in `todo/deep/native-method-accepted-named-declarations.md`.
#
# Note that a *non-zero* count is not automatically a named-argument bug: a
# receiver whose plain call already returns something unstable (`.WHERE`) or
# random is excluded below, but a method whose plain answer diverges from raku
# for an unrelated reason can still show up here. Compare the two runs.

use MONKEY-SEE-NO-EVAL;

my $verbose = so @*ARGS.grep('-v');
# `-t` echoes each probe to stderr before it runs, so an interpreter that dies
# mid-sweep (a stack overflow, a hang) names the probe that killed it.
my $trace   = so @*ARGS.grep('-t');

# A representative receiver per owner named in the row table. Owners with no
# cheap, side-effect-free, deterministic sample (IO::Handle, Supply, the
# RakuAST:: nodes, Backtrace, CallFrame, ...) are deliberately absent: the sweep
# is a comparable metric, not an exhaustive census.
my %RECEIVER =
    'Any'        => 'Any',
    'Mu'         => 'Mu',
    'Nil'        => 'Nil',
    'Bool'       => 'True',
    'Int'        => '42',
    'Num'        => '4.25e0',
    'Rat'        => '0.5',
    'Complex'    => '(1+2i)',
    'Cool'       => '42',
    'Str'        => '"abc"',
    'List'       => '(1, 2, 3)',
    'Array'      => '[1, 2, 3]',
    'Seq'        => '(1, 2, 3).Seq',
    'Hash'       => '{ a => 1 }',
    'Map'        => 'Map.new(("a", 1))',
    'Pair'       => '(a => 1)',
    'Range'      => '(1 .. 5)',
    'Set'        => 'Set.new(1)',
    'SetHash'    => 'SetHash.new(1)',
    'Bag'        => 'Bag.new(1, 1)',
    'BagHash'    => 'BagHash.new(1, 1)',
    'Mix'        => '(a => 1.5).Mix',
    'MixHash'    => '(a => 1.5).MixHash',
    'Blob'       => 'Blob.new(1, 2, 3)',
    'Buf'        => 'Buf.new(1, 2, 3)',
    'Match'      => '("abc" ~~ /b/)',
    'Junction'   => 'any(1, 2)',
    'Date'       => 'Date.new(2026, 1, 1)',
    'DateTime'   => 'DateTime.new(2026, 1, 1, 0, 0, 0)',
    'Duration'   => 'Duration.new(1)',
    'Instant'    => 'Instant.from-posix(1)',
    'IO::Path'   => '"/tmp".IO',
    'Capture'    => '\\(1, 2)',
    'Signature'  => ':(Int $x)',
    'Code'       => '{ $_ }',
    'Block'      => '{ $_ }',
    'Version'    => 'v1.2.3',
    'Uni'        => '"abc".NFC',
    'X::AdHoc'   => 'X::AdHoc.new(payload => "x")',
    ;

# Methods whose plain answer is random, time-dependent, address-dependent or
# has an external side effect. They cannot be compared to themselves, so they
# say nothing about named-blindness.
my $SKIP = set <
    pick roll grab grabpairs WHERE
    print say note put printf print-nl
    exit sleep run shell EVAL EVALFILE
    spurt slurp open unlink mkdir rmdir rename copy move symlink
    lines words getc get readchars read write close watch
    now rand
    line file callframe
    Supply Channel Promise start
>;

sub rows() {
    my $path = 'src/builtins/native_method_row_table.rs';
    my @out;
    for $path.IO.lines -> $line {
        next unless $line ~~ / ^ \s* '(' '"' (<-["]>+) '"' ',' \s* '"' (<-["]>+) '"' ',' \s* (\d+) ',' /;
        @out.push: ($0.Str, $1.Str, $2.Int);
    }
    @out
}

sub arg-shapes($bits) {
    my @shapes;
    @shapes.push: ''            if $bits +& 0b0001;
    @shapes.push: '1', '"a"'    if $bits +& 0b0010;
    @shapes.push: '1, 2'        if $bits +& 0b0100;
    @shapes.push: '', '1'       if $bits +& 0b1000;
    @shapes.unique
}

sub answer($code) {
    my $v = try EVAL($code);
    my $raw = $! ?? "DIED: {$!.Str.lines.head // ''}" !! (try $v.raku) // "UNRAKUABLE";
    # An arity/dispatch error quotes the call back at you, so the probe name
    # itself appears in the message ("Cannot resolve caller Str(Str:D: Str:D,
    # :qqzz9)"). That is the same failure, not a divergence -- erase the probe
    # name before comparing.
    # `Block|3046962412944` / `ObjAt.new("List|4755263302896")`: an object
    # identity, freshly allocated per EVAL, is not an answer.
    $raw.subst(/ ','? \s* ':qqzz9' /, '', :g)
        .subst(/ \s+ ')' /, ')', :g)
        .subst(/ '|' '0x'? <[0..9a..fA..F]>+ /, '|<id>', :g)
}

my $total = 0;
my $diverging = 0;
my @report;

for rows() -> ($owner, $method, $bits) {
    my $recv = %RECEIVER{$owner} // next;
    next if $method (elem) $SKIP;
    for arg-shapes($bits) -> $shape {
        my $plain = "$recv.$method\($shape\)";
        my $withz = $shape ?? "$recv.$method\($shape, :qqzz9)" !! "$recv.$method\(:qqzz9)";
        $total++;
        note $plain if $trace;
        my $a = answer($plain);
        my $b = answer($withz);
        next if $a eq $b;
        $diverging++;
        @report.push: "  $plain\n      plain: $a\n      :qqzz9: $b";
    }
}

say @report.join("\n") if $verbose;
say "probes: $total";
say "not named-blind: $diverging";
