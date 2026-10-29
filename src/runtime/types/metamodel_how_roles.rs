//! The metaroles Rakudo's built-in meta-objects do (`Metamodel::ClassHOW`
//! does `Metamodel::Naming`, `Metamodel::AttributeContainer`, ...), so a
//! type check of a `.HOW` against such a role (`nqp::istype_nd($type.HOW,
//! Metamodel::Stashing)`, what the `Kind` module's refinement does) answers as
//! it does on Rakudo.

/// Each built-in HOW class (without the `Perl6::` prefix and the
/// `Metamodel::` namespace) and the metaroles Rakudo composes into it.
/// Read off Rakudo: `Metamodel::<X>.^roles` unioned with
/// `nqp::istype_nd($type.HOW, Metamodel::<Role>)` over a sample type of each kind.
static HOW_ROLES: &[(&str, &[&str])] = &[
    ("ClassHOW", &["ArrayType", "AttributeContainer", "BUILDALL", "BUILDPLAN", "BoolificationProtocol", "C3MRO", "Composing", "Concretization", "ConcretizationCache", "ContainerSpecProtocol", "DefaultParent", "Documenting", "Finalization", "LanguageRevision", "MROBasedMethodDispatch", "MROBasedTypeChecking", "MetaMethodContainer", "MethodContainer", "Mixins", "MultiMethodContainer", "MultipleInheritance", "Naming", "PrivateMethodContainer", "REPRComposeProtocol", "RoleContainer", "Stashing", "Trusting", "Versioning"]),
    ("PackageHOW", &["Composing", "Documenting", "MethodDelegation", "Naming", "Stashing", "TypePretense"]),
    ("ModuleHOW", &["Composing", "Documenting", "MethodDelegation", "Naming", "Stashing", "TypePretense", "Versioning"]),
    ("GrammarHOW", &["ArrayType", "AttributeContainer", "BUILDALL", "BUILDPLAN", "BoolificationProtocol", "C3MRO", "Composing", "Concretization", "ConcretizationCache", "ContainerSpecProtocol", "DefaultParent", "Documenting", "Finalization", "LanguageRevision", "MROBasedMethodDispatch", "MROBasedTypeChecking", "MetaMethodContainer", "MethodContainer", "Mixins", "MultiMethodContainer", "MultipleInheritance", "Naming", "PrivateMethodContainer", "REPRComposeProtocol", "RoleContainer", "Stashing", "Trusting", "Versioning"]),
    ("ParametricRoleHOW", &["ArrayType", "AttributeContainer", "BUILDALL", "Composing", "Documenting", "LanguageRevision", "MethodContainer", "MultiMethodContainer", "MultipleInheritance", "Naming", "PrivateMethodContainer", "RoleContainer", "RolePunning", "Stashing", "TypePretense", "Versioning"]),
    ("ParametricRoleGroupHOW", &["BUILDALL", "BoolificationProtocol", "Naming", "RolePunning", "Stashing", "TypePretense"]),
    ("ConcreteRoleHOW", &["ArrayType", "AttributeContainer", "BUILDALL", "C3MRO", "Composing", "Concretization", "LanguageRevision", "MethodContainer", "MultiMethodContainer", "MultipleInheritance", "Naming", "PrivateMethodContainer", "RoleContainer"]),
    ("CurriedRoleHOW", &["BUILDALL", "LanguageRevision", "Naming", "RoleContainer", "RolePunning", "TypePretense", "Versioning"]),
    ("EnumHOW", &["AttributeContainer", "BUILDALL", "BUILDPLAN", "BaseType", "BoolificationProtocol", "Composing", "Documenting", "LanguageRevision", "MROBasedMethodDispatch", "MROBasedTypeChecking", "MethodContainer", "Mixins", "MultiMethodContainer", "Naming", "PrivateMethodContainer", "REPRComposeProtocol", "RoleContainer", "Stashing", "Versioning"]),
    ("SubsetHOW", &["BUILDALL", "Documenting", "LanguageRevision", "Naming", "Nominalizable", "Stashing", "Versioning"]),
    ("CoercionHOW", &["LanguageRevision", "Naming", "Nominalizable", "Versioning"]),
    ("NativeHOW", &["BUILDALL", "C3MRO", "Composing", "Documenting", "MROBasedMethodDispatch", "MROBasedTypeChecking", "MultipleInheritance", "Naming", "Stashing", "Versioning"]),
    ("NativeRefHOW", &["BUILDALL", "C3MRO", "Composing", "Documenting", "MROBasedMethodDispatch", "MROBasedTypeChecking", "MultipleInheritance", "Naming", "Stashing", "Versioning"]),
    ("DefiniteHOW", &["Documenting", "Nominalizable"]),
    ("GenericHOW", &["Naming"]),
];

/// Whether the built-in HOW class `how` (`Metamodel::ClassHOW`,
/// `Perl6::Metamodel::ClassHOW`) does the metarole `role`
/// (`Metamodel::Naming`, `Perl6::Metamodel::Naming`). `GrammarHOW` is a
/// `ClassHOW`, so it does that class's roles too.
// Cost: O(r), r = the roles listed for the HOW class (a few dozen).
pub(crate) fn how_does_metarole(how: &str, role: &str) -> bool {
    fn bare(name: &str) -> Option<&str> {
        name.strip_prefix("Perl6::")
            .unwrap_or(name)
            .strip_prefix("Metamodel::")
    }
    let (Some(how), Some(role)) = (bare(how), bare(role)) else {
        return false;
    };
    let does = |class: &str| {
        HOW_ROLES
            .iter()
            .any(|(name, roles)| *name == class && roles.contains(&role))
    };
    does(how) || (how == "GrammarHOW" && does("ClassHOW"))
}

/// The bare name (`Naming`) of a Rakudo metarole or `Archetypes` named by
/// `name` (`Metamodel::Naming`, `Perl6::Metamodel::Naming`): an NQP-level
/// object that is not a `Mu`, unlike the HOW classes and `Primitives`.
// Cost: O(log n), n = the core metamodel names.
pub(crate) fn metarole_bare_name(name: &str) -> Option<&str> {
    let bare = name
        .strip_prefix("Perl6::")
        .unwrap_or(name)
        .strip_prefix("Metamodel::")?;
    (crate::value::is_core_metamodel_name(bare) && !bare.ends_with("HOW"))
        .then_some(bare)
}
