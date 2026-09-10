# No `unit` declarator, and a class named after the module itself: that is what
# registers `EvalLoadedExporter` as this compunit's package, and so puts the
# qualified references below under the #7797 visibility gate. A differently
# named class would not be registered for this module and the gate would stay
# permissive, hiding the bug this fixture exists to catch.
class EvalLoadedExporter {
    has $.tag = 'exported';
    method who() { 'exporter' }
}

our sub plain-helper() is export { EvalLoadedExporter.new.who }

# The shape from the real `Terminal::ANSI::OO`: EXPORT names the module's own
# class, qualified. EXPORT is the module's code, so it must run attributed to
# this compunit no matter who triggered the load.
sub EXPORT($name = 'handle') {
    %( '&' ~ $name => sub () { EvalLoadedExporter.new.tag } )
}
