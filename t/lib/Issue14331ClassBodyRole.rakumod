unit module Issue14331ClassBodyRole;
our class Outer is export {
    my role Inner14331[::T] { method kind { T.^name } }
    method make { Int but Inner14331[Int] }
}
