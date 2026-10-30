use v6.e.PREVIEW;

module ExportMandatory {
    use ExportMandatory::Core;
}

# The operators are copied into the MANDATORY export tag at load time, under
# keys only known at run time (annotations' `EXPORT::MANDATORY`).
my package EXPORT {
    package MANDATORY {
        OUR.WHO.BIND-KEY: .key, .value
            for ExportMandatory::Core::;
    }
}
