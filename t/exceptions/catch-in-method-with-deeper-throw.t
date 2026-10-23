use Test;

# A CATCH whose installing frame is a method frame, with the throw coming from
# a deeper routine: the handler runs on a copy of the method's frame, which
# must not name the original's fixture record (#14770).

plan 6;

class C {
    has $.tag = 'c';

    sub deep { die "boom" }
    sub deeper { deep() }

    method catches {
        CATCH { default { return "caught:" ~ self.tag } }
        deeper();
        'not reached';
    }

    method tries {
        my $r = try { deeper(); 'no' } // 'threw';
        $r ~ ':' ~ self.^name;
    }

    method again {
        # The method's own record must still answer after the handler ran.
        my $first = self.catches;
        $first ~ '/' ~ self.tag ~ '/' ~ ::?CLASS.^name;
    }
}

my $c = C.new;
is $c.catches, 'caught:c', 'CATCH in a method sees self after a deeper throw';
is $c.tries, 'threw:C', 'try in a method, throw from deeper';
is $c.again, 'caught:c/c/C', 'the method keeps its self and class after the handler';
is $c.catches, 'caught:c', 'repeatable';

class D is C { }
is D.new(tag => 'd').again, 'caught:d/d/C', 'inherited method, instance of a subclass';
is D.new.tries, 'threw:D', 'inherited try';
