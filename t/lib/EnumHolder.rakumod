unit class EnumHolder;

our enum EHFormat <EhNone EhMixed EhLists>;

has EHFormat $.fmt is rw;

multi method new(Str() $q) {
    self.new(:fmt(EhLists));
}

method bare-member() {
    # Bare enum member read inside the declaring class's own method.
    EhMixed
}
