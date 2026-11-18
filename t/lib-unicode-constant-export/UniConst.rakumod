unit module UniConst;
class Reals is Range is export {
    method new() { nextwith -Inf, Inf }
    method elems { Inf }
}
my constant \ℝ is export = Reals.new;
my constant Ñ is export = 42;
my constant plain is export = 7;
