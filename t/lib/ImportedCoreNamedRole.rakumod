unit module ImportedCoreNamedRole;
role Expression is export { }
role Variable does Expression is export { has $.name }
class LocalVar does Variable is export { }
