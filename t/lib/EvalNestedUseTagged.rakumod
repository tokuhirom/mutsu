# Plain `is export` routines, a tagged one and an enum: what an EVAL'd `use`
# nested in a block must see without the module being loaded at that spot yet
# (#13441).
unit module EvalNestedUseTagged;
sub plain-fn is export { 1 }
sub tagged-fn is export(:T) { 2 }
sub both-fn is export(:DEFAULT, :M) { 3 }
enum Shade is export <dim bright>;
