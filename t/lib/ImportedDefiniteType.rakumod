use v6;
unit module ImportedDefiniteType;

# A class declared inside a module: its `.^name` is qualified
# (`ImportedDefiniteType::Widget`), and the importing scope sees it under the
# short alias `Widget`.
class Widget is export {
    has Int $.size = 0;
    method area { $!size * $!size }
}
