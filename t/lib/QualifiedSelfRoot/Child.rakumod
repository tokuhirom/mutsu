class QualifiedSelfRoot::Child { }

sub qualified-self-child() is export {
    QualifiedSelfRoot::Child.new.^name
}
