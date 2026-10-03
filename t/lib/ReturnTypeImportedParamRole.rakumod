unit module ReturnTypeImportedParamRole;

role Maybe[::T] is export { }
role Maybe is export { }
role Some[::T] is export { has T $.value }

sub something(::Type $value) is export {
    Some[(Type)].new(:$value) but Maybe[(Type)]
}
