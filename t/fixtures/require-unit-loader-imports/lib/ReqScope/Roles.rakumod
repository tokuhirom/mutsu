unit module ReqScope::Roles;
# A parametric role named like the core `Grammar`, and a class named `Base`,
# exported into whoever uses this module.
role Grammar[$x] is export { }
class Base is export { method origin { 'loader import' } }
