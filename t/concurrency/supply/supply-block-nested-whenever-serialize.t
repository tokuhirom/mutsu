use Test;

# A supply block runs one `whenever` body at a time, and its `emit`s reach the
# tap inside that body, so tap callbacks never overlap. That must also hold for
# a `whenever` registered from inside another body, whose source is a nested
# supply block fed from a `start` thread, while a sibling body on another
# thread is blocked waiting for that thread (#11307, Cro::WebSocket's
# MessageSerializer).

plan 4;

sub run-case(&wait-for) {
    my $p = Promise.new;
    my $in = Supplier.new;
    my $s = supply {
        whenever $in.Supply -> $m {
            if $m == 1 {
                my $inner = supply {
                    whenever start { sleep 0.05; 'x' } -> $ {
                        $p.keep('p');
                        emit 'A1';
                        emit 'A2';
                    }
                };
                whenever $inner { emit $_ }
            }
            else {
                emit 'B' ~ wait-for($p);
            }
        }
    }
    my @log;
    my $log-lock = Lock.new;
    my $all-seen = Promise.new;
    my $seen = 0;
    $s.tap: -> $v {
        $log-lock.protect: { @log.push("start $v") };
        sleep 0.1;
        $log-lock.protect: {
            @log.push("end $v");
            $all-seen.keep if ++$seen == 3;
        };
    };
    $in.emit(1);
    $in.emit(2);
    await Promise.anyof($all-seen, Promise.in(10));
    @log
}

for ('.result', { .result }), ('await', { await $_ }) -> ($name, &wait-for) {
    # Named apart from run-case's own @log: see #11345.
    my @seen = run-case(&wait-for);
    is @seen.elems, 6, "$name: every value reached the tap";
    my @pairs = @seen.map(-> $a, $b { $a.subst('start ', '') eq $b.subst('end ', '') });
    ok @pairs.elems == 3 && all(@pairs),
        "$name: tap callbacks never overlap (@seen.join(', '))";
}
