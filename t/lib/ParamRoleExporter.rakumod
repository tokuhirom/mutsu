unit module ParamRoleExporter;

role Box[::T] is export {
    has $.n;
    method new(::?CLASS:U: Int $n) { self.bless(:$n) }
    method from-size(::?CLASS:U: Int :$size) { self.bless(:n($size)) }
}
