# A module whose `sub EXPORT` takes import arguments, and whose operators
# are copied into the MANDATORY export tag at load time under keys only known
# at run time (annotations, #18091). A `use ExportMandatoryHook <x>` must
# still load at parse time, so the parser knows the operators.
module ExportMandatoryHook {
    use ExportMandatoryHook::Core;
}

my package EXPORT {
    package MANDATORY {
        OUR.WHO.BIND-KEY: .key, .value
            for ExportMandatoryHook::Core::;
    }
}

sub EXPORT(*@args) { Map.new }
