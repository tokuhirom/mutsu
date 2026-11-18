# Inherits the grammar its own body `use`s (the CSS::Specification::Extended shape).
grammar AlsoIsStub::Ext {
    use AlsoIsStub;
    also is AlsoIsStub;
    token word { \d+ }
}
