unit module DoBindShadowInner;
use nqp;

# Binds `$!do` on its own `shadow-do` at load time, as upstream NativeCall's
# `is native` does at trait time.
our sub shadow-do($a, $b, $c) is export { "inner($a, $b, $c)" }

role Replaced {
    method install() {
        my $replacement := -> |c { "replaced({c.list.elems})" };
        nqp::bindattr(self, Code, '$!do', nqp::getattr($replacement, Code, '$!do'));
    }
}
&shadow-do does Replaced;
&shadow-do.install;
