unit module ProtoMultiMismatchedConsumer;

use ProtoMultiMismatchedUnitName;

our sub call-mismatched-export($object, $length) is export {
    mismatched-export-capture($object, $length)
}
