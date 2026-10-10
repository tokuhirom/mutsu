# Code.Capture and Code.clone are method rows

Code.Capture/clone are pure rows of the method table (ADR-12523 slice 5). Regex.clone and &say.clone no longer die with "No such method", and &say.Capture throws X::Cannot::Capture like every other code object.
