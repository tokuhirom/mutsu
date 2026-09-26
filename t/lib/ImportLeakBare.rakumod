use ImportLeakProvider;
class ImportLeakBare { method e { leak-ex(1) } }
sub bare-leak-call is export { leak-ex(1) }
