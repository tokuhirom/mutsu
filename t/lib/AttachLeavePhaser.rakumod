# A module whose EXPORT attaches a LEAVE phaser to the scope that `use`s it,
# through the RakuAST resolver `$*R` -- the FINALIZER idiom (FINALIZER 0.0.10).
# Used by t/modules/import-export/export-hook-attaches-leave-phaser.t; the
# phaser logs into the dynamic `@*ATTACH-LOG` of whoever left the scope.
use experimental :rakuast;

# The phaser node wraps an already-compiled callable and reports it as its
# code object, exactly like FINALIZER's own `LeavePhaser`.
my class LeavePhaser is RakuAST::StatementPrefix::Phaser::Leave {
    has &!code;
    method new(&code) {
        my $phaser := callwith(RakuAST::Block.new);
        $phaser!set-code(&code);
        $phaser
    }
    method !set-code(&code --> Nil) { &!code := &code }
    method meta-object() { &!code }
}

sub EXPORT($name = 'anon') {
    ($*R.find-attach-target('block') // $*R.find-attach-target('compunit'))
      .add-leave-phaser: LeavePhaser.new({ @*ATTACH-LOG.push: "leave $name" });
    {}
}
