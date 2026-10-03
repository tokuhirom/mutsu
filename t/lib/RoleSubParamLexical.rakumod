use nqp;

# The shape of upstream NativeCall's `Native` role (#11528, #11203): a role
# parameterised on the routine it is mixed into, with a role-body INIT
# lexical, applied by a trait candidate returned from `sub EXPORT`, and whose
# private method is later reached through a body rebound via `$!do`.
our module RoleSubParamLexical {
    multi trait_mod:<is>(Routine $r, :$unused-trait!) is export(:DEFAULT, :traits) { }

    our role Wrapped[Routine $routine, $tag where Str | Callable] {
        has int $!calls;
        has str $!name;
        INIT my Lock $guard .= new;
        my $plain = 'plain';

        method !bump() {
            $guard.protect: { $!calls = $!calls + 1 }
        }

        method setup-body() {
            $!name = self.name;
            my $replacement := -> |c {
                self!bump;
                "{$guard.^name} $plain calls=$!calls name=$!name"
            };
            nqp::bindattr(self, Code, '$!do', nqp::getattr($replacement, Code, '$!do'));
        }
    }
}

sub EXPORT(|) {
    my $trait := multi trait_mod:<is>(Routine $r, :$wrapped!) {
        $r does RoleSubParamLexical::Wrapped[$r, Str];
        $r.setup-body;
    };
    Map.new('&trait_mod:<is>' => $trait.dispatcher);
}
