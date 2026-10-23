class AugmentConstReader { }
my constant @list = <a b>;
my constant $scalar = 5;
my constant %hash = a => 1;
use MONKEY-TYPING;
augment class AugmentConstReader {
    method list { @list }
    method scalar { $scalar }
    method hash-a { %hash<a> }
}
