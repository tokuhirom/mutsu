use Test;

plan 2;

# A `supply { }` body's own `my %h` is boxed into a shared `ContainerRef`
# cell (`share_supply_block_lexicals`) so every dispatch of the enclosing
# `whenever` callback mutates the SAME binding instead of a private
# per-dispatch snapshot. The `%h{$k} = $v` write-through-shared-cell fast
# path (`assign_hash_elem_to_shared_var`) only recognised a bare
# `ValueView::Hash`, not one already boxed into such a cell — once a nested
# `whenever` (e.g. `whenever $cancellation-promise { ... }`) registered
# mid-event and activated `shared_vars_active`, later element writes fell
# through to that path, silently treated the boxed cell as an *unshared*
# hash, and reinstalled a brand-new unboxed Hash that lost every key
# written before the nested registration. See
# todo/deep/nested-whenever-registration-clobbers-sibling-event-aggregate-writes.md
# (root cause of the Cro::HTTP2 `http2-request-parser.rakutest` "check 4"
# failure — HTTP/2 stream demux losing a sibling stream's state).

sub streams-after-nested-whenever(Bool $nested-first) {
    my $trigger = Supplier.new;
    my $done = Promise.new;
    my @seen;
    my $s = supply {
        my %streams;
        whenever $trigger.Supply -> $sid {
            unless %streams{$sid}:exists {
                if $nested-first {
                    my $cancellation = Promise.new;
                    whenever $cancellation { note "cancelled $sid" }
                    %streams{$sid} = "S$sid";
                } else {
                    %streams{$sid} = "S$sid";
                    my $cancellation = Promise.new;
                    whenever $cancellation { note "cancelled $sid" }
                }
            }
            @seen.push(%streams.keys.sort.join(','));
            $done.keep if $sid == 5;
        }
    };
    $s.tap: -> $v { };
    $trigger.emit(3);
    $trigger.emit(5);
    await Promise.anyof($done, Promise.in(3));
    @seen[*-1];
}

is streams-after-nested-whenever(False), '3,5',
    'nested whenever registered AFTER the hash write does not clobber the sibling key';
is streams-after-nested-whenever(True), '3,5',
    'nested whenever registered BEFORE the hash write does not clobber the sibling key';
