use Test;

# A CONTROL block whose `when CX::Warn { ...; .resume }` arm is followed by a
# `default` arm resumes an op-raised warning at its raise site. `when` is
# first-match, so a warning never reaches the `default` arm however that arm
# ends (#9425). This is CodeUnit's `CodeUnit.eval` shape.

plan 7;

sub concat-warns() {
    CONTROL {
        when CX::Warn { .resume }
        default { return 'default' }
    }
    my $x;
    my $y = "a" ~ $x;
    'after'
}
is concat-warns(), 'after', 'an op-raised warning resumes past a trailing default arm';

my @seen;
sub records-warning() {
    CONTROL {
        when CX::Warn { @seen.push: 'caught'; .resume }
        default { @seen.push: 'default' }
    }
    my $x;
    my $y = "a" ~ $x;
    @seen.push: 'after';
    5
}
is records-warning(), 5, 'the routine returns its own value';
is @seen, <caught after>, 'the when arm ran, then the code after the raise site';

sub other-control-first() {
    CONTROL {
        when CX::Done { return 'done' }
        when CX::Warn { .resume }
        default { return 'default' }
    }
    my $x;
    my $y = "a" ~ $x;
    'after'
}
is other-control-first(), 'after', 'an arm for another CX type before the warn arm is skipped';

class Evaluator {
    method ev($code) {
        CONTROL {
            when CX::Warn { .resume }
            default { return Nil }
        }
        use MONKEY-SEE-NO-EVAL;
        EVAL $code
    }
}
is Evaluator.ev(q/my $x; $x + 1/), 1, 'EVAL in a method: numeric warning resumes with the value';
is Evaluator.ev(q/my $x; "a" ~ $x ~ "b"/), 'ab', 'EVAL in a method: string warning resumes with the value';

sub default-does-not-resume() {
    CONTROL {
        default { return 'default' }
        when CX::Warn { .resume }
    }
    my $x;
    my $y = "a" ~ $x;
    'after'
}
is default-does-not-resume(), 'default', 'a default arm that comes first still decides';
