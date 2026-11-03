# A method refers to an exception class the file declares further down, by its
# qualified name (Directory 0.0.10 does exactly this with X::Directory::FileExists).
class PostDeclaredQualified {
    method fail-with($path) {
        die (X::PostDeclaredQualified::Late.new(:$path));
    }
    method fine { 'fine' }
}

class X::PostDeclaredQualified::Late is Exception {
    has $.path;
    method message { "late: $!path" }
}
