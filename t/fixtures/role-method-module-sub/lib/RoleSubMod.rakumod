unit module RoleSubMod;

sub helper() is export { "h" }

role Greets is export {
    method greet { helper() ~ "!" }
}
