sub default-mode(--> Str) { 'direct' }

class Issue7733::Conf is export {
    has Str $.attribute-default = default-mode();
    has Str $.build-default;

    submethod BUILD(Str :$!build-default = default-mode()) { }
}
