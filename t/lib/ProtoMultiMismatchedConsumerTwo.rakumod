unit module ProtoMultiMismatchedConsumerTwo;

use ProtoMultiMismatchedUnitName;

our sub call-mismatched-export-two($object, $length) is export {
    mismatched-export-capture($object, $length)
}
