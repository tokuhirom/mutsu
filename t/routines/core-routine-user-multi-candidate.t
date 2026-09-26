use v6;
use Test;

plan 11;

# A proto-less `multi sub` named like a core routine adds a candidate to
# CORE's proto rather than replacing the routine: the user candidate wins
# when it matches, and every other call still reaches the builtin. This holds
# for the statement forms of `die`/`fail` (which have their own parser) as
# much as for a parenthesised call, and for an imported multi -- including
# one imported with `-M` -- as much as for a local one. Reduced from the
# `Die` distribution (t/01-die.t).

{
    my @log;
    multi sub die(Str:D $m where * eq 'mine') { @log.push("user:$m") }
    die 'mine';
    is-deeply @log, ['user:mine'], 'statement-form die calls a matching user multi';
    throws-like { die 'other' }, X::AdHoc, message => 'other',
        'statement-form die falls back to the builtin when no candidate matches';
    throws-like { die('other') }, X::AdHoc, message => 'other',
        'parenthesised die falls back to the builtin too';
    throws-like { die }, X::AdHoc, message => /Died/,
        'argumentless die still reaches the builtin';
}

{
    my @log;
    multi sub fail(Str:D $m where * eq 'mine') { @log.push("user:$m") }
    sub f { fail 'mine'; 'went on' }
    is f(), 'went on', 'statement-form fail calls a matching user multi';
    sub g { fail 'other' }
    my $r = g();
    ok $r ~~ Failure, 'statement-form fail falls back to the builtin';
    is $r.exception.message, 'other', '... carrying the message';
}

{
    my @log;
    multi sub note(Str:D $m where * eq 'mine') { @log.push("user:$m") }
    note 'mine';
    is-deeply @log, ['user:mine'], 'user multi note wins when it matches';
}

my $lib = $*PROGRAM.parent.parent.add('lib').absolute;

sub run-mutsu(*@args) {
    my $proc = run $*EXECUTABLE, "-I$lib", |@args, :out, :err;
    ($proc.out.slurp(:close), $proc.err.slurp(:close), $proc.exitcode)
}

{
    my ($out, $err, $code) = run-mutsu('-e', 'multi sub say(Str:D $ where * eq "q") { print "user\n" }; say "a"; say "q"');
    is $out, "a\nuser\n", 'user multi say: builtin for the non-matching call, user for the matching one';
}

{
    my ($out, $err, $code) = run-mutsu('-e', 'use NewlineDie; say "hi"; die "foo\n"');
    is-deeply ($out, $err, $code), ("hi\n", "foo\n", 1),
        'an imported multi die shadows the statement form';
}

{
    my ($out, $err, $code) = run-mutsu('-MNewlineDie', '-e', 'say "hi"; die "foo\n"');
    is-deeply ($out, $err, $code), ("hi\n", "foo\n", 1),
        'a multi die imported with -M shadows the statement form';
}
