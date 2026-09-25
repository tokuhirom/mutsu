use Test;

# A CONTROL handler that is not provably resume-safe but contains a `.resume`
# (here, a conditional one) runs INLINE at the warning's raise site, the way
# rakudo runs every CONTROL handler on top of the stack. So `.resume` reaches
# an op-raised warning and a warning raised in a callee, and a handler that
# does not resume ends its region without running twice (#9469).

plan 13;

my $flag = True;
my @log;

sub f0() {
    CONTROL { when CX::Warn { @log.push: 'c'; .resume if $flag; @log.push: 'not resumed' } }
    my $x;
    my $y = "a" ~ $x;
    @log.push: 'after';
    5
}
@log = ();
is f0(), 5, 'an op-raised warning in the same frame resumes';
is-deeply @log, ['c', 'after'], '...and the rest of the frame runs';

sub inner-op() { my $x; my $y = "a" ~ $x; @log.push: 'in-after'; 3 }
sub f() {
    CONTROL { when CX::Warn { @log.push: 'c'; .resume if $flag; @log.push: 'not resumed' } }
    my $r = inner-op();
    @log.push: "after $r";
    5
}
@log = ();
is f(), 5, 'an op-raised warning in a callee resumes';
is-deeply @log, ['c', 'in-after', 'after 3'], '...inside the callee';

sub inner-warn() { warn "ww"; @log.push: 'in-after2'; 3 }
sub g() {
    CONTROL { when CX::Warn { @log.push: 'c'; .resume if $flag; @log.push: 'not resumed' } }
    my $r = inner-warn();
    @log.push: "after $r";
    5
}
@log = ();
is g(), 5, 'a warn in a callee resumes';
is-deeply @log, ['c', 'in-after2', 'after 3'], '...inside the callee';

$flag = False;
@log = ();
is f(), Nil, 'a handler that does not resume ends its region';
is-deeply @log, ['c', 'not resumed'], '...running the handler exactly once';

# The warning passes a declining handler and is resumed by an outer one.
sub k() {
    CONTROL { when CX::Take { @log.push: 'never'; .resume } }
    my $x;
    my $y = "a" ~ $x;
    @log.push: 'k after';
    7
}
sub h() {
    CONTROL { when CX::Warn { @log.push: 'outer'; .resume } }
    my $r = k();
    @log.push: "h after $r";
    6
}
@log = ();
is h(), 6, 'an outer handler resumes past a declining inner one';
is-deeply @log, ['outer', 'k after', 'h after 7'], '...at the raise site';

# Every handler declines: the default handler reports and resumes.
sub m() {
    CONTROL { when CX::Take { .resume } }
    quietly { warn "mm" };
    warn "mm";
    @log.push: 'm after';
    8
}
@log = ();
is m(), 8, 'a warning every handler declines is resumed by the default handler';

# A handler that stops resuming after a count keeps its lexical writes.
my $n = 0;
sub cnt() {
    CONTROL { when CX::Warn { $n++; .resume if $n < 3 } }
    for ^5 { warn "w$_"; @log.push: "iter $_" }
    'done'
}
@log = ();
is cnt(), Nil, 'the third warning ends the region';
is-deeply [$n, |@log], [3, 'iter 0', 'iter 1'], '...after two resumed iterations';
