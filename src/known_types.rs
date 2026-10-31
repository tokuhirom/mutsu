//! Whether a name is a built-in type constraint. A pure table, below the parser,
//! so the RakuAST frontend can ask it without naming the runtime (ADR-15760 slice P).

// Cost: O(k), k = length of `constraint` (one match over a fixed set of names).
pub(crate) fn is_known_type_constraint(constraint: &str) -> bool {
    if crate::native_types::is_native_int_type(constraint) {
        return true;
    }
    // Handle parameterized types like Array[Int], Hash[Str], etc. `ends_with`
    // is O(1) and rejects every unparameterized name, so it gates the `[` scan
    // rather than following it (#7696).
    if constraint.ends_with(']')
        && let Some((base, _)) = crate::str_scan::split_once_bracket(constraint)
        && is_known_type_constraint(base)
    {
        return true;
    }
    matches!(
        constraint,
        "Int"
            | "UInt"
            | "Num"
            | "Str"
            | "Bool"
            | "Array"
            | "Hash"
            | "Rat"
            | "FatRat"
            | "Complex"
            | "Real"
            | "Numeric"
            | "Rational"
            | "atomicint"
            | "int"
            | "num"
            | "num32"
            | "num64"
            | "str"
            // Core types
            | "Mu"
            | "Any"
            | "Cool"
            | "Enumeration"
            | "Nil"
            | "Pair"
            | "Map"
            | "List"
            | "Slip"
            | "Seq"
            | "Range"
            | "Set"
            | "SetHash"
            | "Bag"
            | "BagHash"
            | "Mix"
            | "MixHash"
            | "QuantHash"
            | "Blob"
            | "Buf"
            // Native sized buffer aliases: blob8 == Blob[uint8], buf16 == Buf[uint16], etc.
            // (utf8/utf16/utf32 are already listed below.)
            | "blob"
            | "buf"
            | "blob8"
            | "blob16"
            | "blob32"
            | "blob64"
            | "buf8"
            | "buf16"
            | "buf32"
            | "buf64"
            | "Junction"
            | "Match"
            | "Cursor"
            | "Regex"
            | "Block"
            | "Callable"
            | "Code"
            | "Sub"
            | "Method"
            | "Submethod"
            | "Routine"
            | "Capture"
            | "Signature"
            | "Parameter"
            | "Whatever"
            | "WhateverCode"
            | "HyperWhatever"
            | "Format"
            | "Version"
            | "Date"
            | "DateTime"
            | "Instant"
            | "Duration"
            | "Failure"
            | "Exception"
            | "Backtrace"
            | "Scalar"
            | "Proxy"
            | "Label"
            | "Grammar"
            // Allomorphs
            | "Allomorph"
            | "IntStr"
            | "NumStr"
            | "RatStr"
            | "ComplexStr"
            // Meta types
            | "Attribute"
            | "AST"
            | "Compiler"
            | "Distro"
            | "Kernel"
            | "Macro"
            | "ObjAt"
            | "ValueObjAt"
            | "Stash"
            | "PseudoStash"
            | "Raku"
            | "Perl"
            | "VM"
            | "Variable"
            // Array-like
            | "array"
            | "Hashray"
            // Numeric subtypes
            | "byte"
            | "uint"
            // IO types
            | "IO"
            // Ref types
            | "IntAttrRef"
            | "IntLexRef"
            | "IntPosRef"
            | "NumAttrRef"
            | "NumLexRef"
            | "NumPosRef"
            | "StrAttrRef"
            | "StrLexRef"
            | "StrPosRef"
            | "UIntAttrRef"
            | "UIntLexRef"
            | "UIntPosRef"
            | "ScalarVAR"
            // String types
            | "StrDistance"
            // Iteration
            | "IterationBuffer"
            // Deprecation
            | "Deprecation"
            | "Distribution"
            // Operators
            | "Operator"
            | "OperatorProperties"
            // Unicode types
            | "Unicode"
            | "Uni"
            | "NFC"
            | "NFD"
            | "NFKC"
            | "NFKD"
            | "utf8"
            | "utf16"
            | "utf32"
            | "SignedBlob"
            | "UnsignedBlob"
            // Concurrent types
            | "Cancellation"
            | "Channel"
            | "CurrentThreadScheduler"
            | "Promise"
            | "PromiseStatus"
            | "Semaphore"
            | "Supplier"
            | "Supply"
            | "Tap"
            | "Thread"
            | "ThreadPoolScheduler"
            | "Scheduler"
            | "Telemetry"
            // Hyper/Parallel
            | "HyperConfiguration"
            | "HyperSeq"
            | "ParallelSequence"
            | "RaceSeq"
            // Slang
            | "Slang"
            // Lock
            | "Lock"
            // CallFrame
            | "CallFrame"
            // CompUnit
            | "CompUnit"
            // Core roles
            | "Associative"
            | "Positional"
            | "PositionalBindFailover"
            | "Iterable"
            | "Iterator"
            | "PredictiveIterator"
            | "Sequence"
            | "Stringy"
            | "Baggy"
            | "Mixy"
            | "Setty"
            | "Dateish"
            | "Systemic"
            | "Encoding"
            | "Formatter"
            | "ForeignCode"
            | "Collation"
            // Process/system
            | "Proc"
            | "Signal"
            | "Order"
            | "Endian"
            // RakuAST (top-level namespace only; RakuAST::* compound names are
            // not covered here — see is_known_compound_type)
            | "RakuAST"
    )
}
