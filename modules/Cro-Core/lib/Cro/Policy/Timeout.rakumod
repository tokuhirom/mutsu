class X::Cro::Policy::Timeout::InvalidTimeout is Exception {
    has $.kind;
    has @.kinds;

    method message {
        "Invalid kind of timeout, expected one of [@!kinds.join(', ')], got: $!kind";
    }
}

class X::Cro::Policy::Timeout::InvalidTimeoutValue is Exception {
    has $.kind;
    has $.value;

    method message {
        "Invalid timeout value passed for $!kind, got: $!value";
    }
}

role X::Cro::Policy::Timeout is Exception {
    has Str $.phase is required;
}

role Cro::Policy::Timeout[%phase-defaults] {
    has Real() $.total is required;
    has Real() %.phases;

    submethod BUILD(Real:D() :$!total, *%phases --> Nil) {
        for %phase-defaults.kv -> $kind, $default {
            my $supplied-value = %phases{$kind}:delete;
            if $supplied-value ~~ Real {
                %!phases{$kind} = Real($supplied-value);
            } else {
                %!phases{$kind} = Real($default);
                with $supplied-value {
                    die X::Cro::Policy::Timeout::InvalidTimeoutValue.new(value => $_, :$kind);
                }
            }
        }
        if %phases {
            for %phases.keys {
                die X::Cro::Policy::Timeout::InvalidTimeout.new(
                        kinds => %phase-defaults.keys,
                        kind => $_);
            }
        }
    }

    #| Request the remaining amount of time available for the given phase,
    #| given the specified amount of time has elapsed since the start of
    #| the request.
    method get-timeout(Real:D() $time-since-start, Str:D $phase --> Real:D) {
        if $!total ~~ Inf {
            %!phases{$phase};
        } else {
            my $time-left = $!total - $time-since-start;
            min %!phases{$phase}, $time-left < 0 ?? 0 !! $time-left;
        }
    }
}
