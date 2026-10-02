use Test;

# #10921: a Seq argument that fails a typed `@` parameter's constraint is
# reported as the Seq the caller passed, not the List view the binder
# rebinds it to.

plan 8;

sub h(Int @a) { }

{
    h((1, 2).Seq);
    CATCH {
        default {
            isa-ok $_, X::TypeCheck::Binding::Parameter, 'Seq arg: binding exception';
            is .message,
                "Type check failed in binding to parameter '@a'; expected Positional[Int] but got Seq ((1, 2).Seq)",
                'Seq arg: message names Seq';
            isa-ok .got, Seq, 'Seq arg: .got is the Seq';
        }
    }
}

{
    h((1..3).map(* + 1));
    CATCH {
        default {
            like .message, /'but got Seq ((2, 3, 4).Seq)'/, 'map Seq arg: message names Seq';
            isa-ok .got, Seq, 'map Seq arg: .got is the Seq';
        }
    }
}

{
    h(gather { take 1 });
    CATCH {
        default {
            like .message, /'but got Seq'/, 'gather arg: message names Seq';
            isa-ok .got, Seq, 'gather arg: .got is the Seq';
        }
    }
}

# An untyped `@` parameter still binds a Seq as a List.
sub k(@a) { @a.WHAT }
is k((1, 2).Seq).^name, 'List', 'untyped @ param binds a Seq as a List';
