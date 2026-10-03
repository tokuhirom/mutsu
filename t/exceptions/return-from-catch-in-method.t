use v6;
use Test;

# A `return` reaching a method through a try/CATCH boundary must return from
# that method. The method's frame is identified by a per-invocation callable
# id, not a routine registration id, and the "is the return target still
# live?" check at the boundary only knew the latter, so the return turned
# into X::ControlFlow::Return ("Attempt to return outside of
# immediately-enclosing Routine"). Found through HTTP::Tiny's `!request`,
# whose CATCH returns a 599 response for an exception raised in a callback.

plan 7;

class H { method boom { die 'boom' } }

class U {
    method via-block {
        CATCH { default { return "caught:" ~ .message } }
        my $c = -> $ { die 'boom' };
        $c(1);
        'fell through'
    }
    method go-private { self!private }
    method !private {
        CATCH { default { return "caught:" ~ .message } }
        my $c = -> $ { die 'boom' };
        $c(1);
        'fell through'
    }
    method via-stored-callback {
        CATCH { default { return %( status => 599, message => .message ) } }
        my $writer = -> $ { die 'Something terrible happened' };
        my $h = class { has $.writer; method write($b) { $.writer.($b) } }.new(:$writer);
        $h.write('x');
        %( status => 200 )
    }
    method block-return-through-try {
        try { my $c = -> { return 6 }; $c() };
        1
    }
    method via-other-method {
        CATCH { default { return "caught:" ~ .message } }
        H.boom;
        'fell through'
    }
}

is U.via-block, 'caught:boom', 'CATCH return after a die in a called block';
is U.go-private, 'caught:boom', 'same, in a private method';
is-deeply U.via-stored-callback, %( status => 599, message => 'Something terrible happened' ),
    'CATCH return after a die in a stored callback';
is U.block-return-through-try, 6, 'a block return crosses a try inside a method';
is U.via-other-method, 'caught:boom', 'CATCH return after a die in another method';

sub plain {
    CATCH { default { return "caught:" ~ .message } }
    my $c = -> $ { die 'boom' };
    $c(1);
    'fell through'
}
is plain(), 'caught:boom', 'subs keep working';

sub outer-of-method { U.via-block ~ '|outer' }
is outer-of-method(), 'caught:boom|outer', 'the return stops at the method, not its caller';
