# Like AttachLeavePhaser, but the phaser reads a dynamic variable the module
# exports in the same EXPORT -- declared AFTER the phaser's closure is made
# (the FINALIZER 0.0.10 idiom: `my $*FINALIZER` after `{ $*FINALIZER.FINALIZE }`).
# Used by t/modules/import-export/export-leave-phaser-sees-exported-dynamic.t.
use experimental :rakuast;

class Holder { has $.tag is rw }

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

sub EXPORT() {
    my &LEAVE-IT := { @*PHASER-LOG.push: $*HOLDER.defined ?? "holder " ~ $*HOLDER.tag !! "no holder" }
    ($*R.find-attach-target('block') // $*R.find-attach-target('compunit'))
      .add-leave-phaser: LeavePhaser.new(&LEAVE-IT);
    my %export;
    %export.BIND-KEY('$*HOLDER', my $*HOLDER = Holder.new(tag => 'exported'));
    %export
}
