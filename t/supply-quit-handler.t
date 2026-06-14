# Supply `whenever ... { QUIT {...} }` handler dispatch.
#
# The QUIT phaser body now runs as VM-native compiled bytecode (via
# VM::call_supply_quit_handler -> call_react_callback), not the tree-walking
# call_sub_value. These cases pin the observable behaviour: a matched
# when/default handles the quit (the supply completes via done, no quit fires);
# an unmatched QUIT lets the quit propagate; a matched handler may emit and call
# done. Mirrors roast/S17-supply/syntax.t's QUIT cluster.
use Test;

plan 7;

my class OMGBears is Exception { }
my class Useless  is Exception { }

# 1-2: QUIT { default {} } handles any exception -> supply is done, no quit.
{
    my $trigger = Supplier.new;
    my $s = supply {
        whenever $trigger {
            QUIT { default { } }
        }
    }
    my $dones = 0;
    my $quits = 0;
    $s.tap(done => { $dones++ }, quit => { $quits++ });
    $trigger.quit(Exception.new);
    is $dones, 1, 'QUIT default handles the exception -> supply completes (done)';
    is $quits, 0, '...and no quit is propagated';
}

# 3-4: QUIT { when <other> {} } does NOT match -> the quit still propagates.
{
    my $trigger = Supplier.new;
    my $s = supply {
        whenever $trigger {
            QUIT { when Useless { } }
        }
    }
    my $dones = 0;
    my $quits = 0;
    $s.tap(done => { $dones++ }, quit => { $quits++ });
    $trigger.quit(Exception.new);
    is $quits, 1, 'unmatched QUIT lets the quit propagate';
    is $dones, 0, '...and done is not run';
}

# 5-7: a matched QUIT may emit and call done; values up to the quit plus the
# QUIT-emitted value all come through.
{
    my $trigger = Supplier.new;
    my $s = supply {
        whenever $trigger {
            emit $_;
            QUIT {
                when OMGBears {
                    emit 'Run you fools!';
                    done;
                }
            }
        }
    }
    my @collected;
    my $dones = 0;
    my $quits = 0;
    $s.tap({ @collected.push($_) }, done => { $dones++ }, quit => { $quits++ });
    $trigger.emit('Something is coming');
    $trigger.emit('Something big');
    $trigger.quit(OMGBears.new);
    is @collected, ['Something is coming', 'Something big', 'Run you fools!'],
        'emits up to the quit, plus the QUIT-block emit, all arrive in order';
    is $dones, 1, 'done inside the matched QUIT block fired';
    is $quits, 0, 'a handled exception does not produce a quit';
}
