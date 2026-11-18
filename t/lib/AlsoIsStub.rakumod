# A grammar a module's `also is` names from inside its body, after the body's
# own `use` (t/modules/compunit/precomp-also-is-body-use-stub-package.t).
unit grammar AlsoIsStub;
token TOP { <word> }
token word { \w+ }
