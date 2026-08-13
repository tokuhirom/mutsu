[![Actions Status](https://github.com/japhb/CBOR-Simple/actions/workflows/test.yml/badge.svg)](https://github.com/japhb/CBOR-Simple/actions)

NAME
====

CBOR::Simple - Simple codec for the CBOR serialization format

SYNOPSIS
========

```raku
use CBOR::Simple;

# Encode a Raku value to CBOR, or vice-versa
my $cbor = cbor-encode($value);
my $val1 = cbor-decode($cbor);               # Fails if more data past first decoded value
my $val2 = cbor-decode($cbor, my $pos = 0);  # Updates $pos after decoding first value

# By default, cbor-decode() marks partially corrupt parsed structures with
# Failure nodes at the point of corruption
my $bad  = cbor-decode(buf8.new(0x81 xx 3));  # [[[Failure]]]

# Callers can instead force throwing exceptions on any error
my $*CBOR_SIMPLE_FATAL_ERRORS = True;
my $bad  = cbor-decode(buf8.new(0x81 xx 3));  # BOOM!

# Decode CBOR into diagnostic text, used for checking encodings and complex structures
my $diag = cbor-diagnostic($cbor);

# Force the encoder to tag a value with a particular tag number
my $tagged = CBOR::Simple::Tagged.new(:$tag-number, :$value);
my $cbor   = cbor-encode($tagged);
```

DESCRIPTION
===========

`CBOR::Simple` is an easy-to-use implementation of the core functionality of the [CBOR serialization format](https://cbor.io/), implementing the standard as of [RFC 8949](https://tools.ietf.org/html/rfc8949), plus a collection of common tag extensions as described below in [TAG IMPLEMENTATION STATUS](#tag-implementation-status).

PERFORMANCE
-----------

`CBOR::Simple` is one of the fastest data structure serialization codecs available for Raku. It is comparable in round-trip speed to `JSON::Fast` for data structures that are the most JSON-friendly. For all other cases tested, `CBOR::Simple` produces smaller, higher fidelity encodings, faster. For more detail, and comparison with other Raku serialization codecs, see [serializer-perf](https://github.com/japhb/serializer-perf).

NYI
---

Currently known NOT to work:

  * Any tag marked '✘' (valid but not yet supported) or 'D' (deprecated spec) in the ENCODE or DECODE column of the Tag Status Details table below, or any tag not explicitly listed therein, will be treated as an opaque tagged value rather than treated as a native type.

  * Packed arrays of 128-bit floats (num128); these are not supported in Rakudo yet.

  * Encoding *finite* 16-bit floats (num16); encoding 16-bit NaN and ±Inf, as well as decoding any num16 all work. This is a performance tradeoff rather than a technical limitation; detecting whether a finite num32 can be shrunk to 16 bits without losing information is costly and rarely results in space savings except in trivial cases (e.g. Nums containing only small integers).

TAG CONTENT STRICTNESS
----------------------

When encoding, `CBOR::Simple` makes every attempt to encode tagged content strictly within the tag standards as written, always producing spec-compliant encoded values.

When decoding, `CBOR::Simple` will often slightly relax the allowed content types in tagged content, especially when later tag proposals made no change other than to extend the allowed content types and allocate a new tag number for that. In the extension case `CBOR::Simple` is likely to allow both the old and new tag to accept the same content domain when decoding.

For example, when encoding `CBOR::Simple` will always encode `Instant` or `DateTime` as a CBOR epoch-based date/time (tag 1), using standard integer or floating point content data. But when *decoding*, `CBOR::Simple` will accept any content that decodes properly as a Raku `Real` value -- and in particular will handle a CBOR Rational (tag 30) as another valid content type.

DATE, DATETIME, INSTANT
-----------------------

Raku's builtin time handling is richer than the default CBOR data model (though certain tag extensions improve this), so the following mappings apply:

  * Encoding

    * `Instant` and `DateTime` are both written as tag 1 (epoch-based date/time) with integer (if lossless) or floating point content.

    * Other `Dateish` are written as tag 100 (RFC 8943 days since 1970-01-01).

  * Decoding

    * Tag 0 (date/time string) is parsed as a `DateTime`.

    * Tag 1 (epoch-based date/time) is parsed via `Instant.from-posix()`, and handles any Real type in the tag content.

    * Tag 100 (days since 1970-01-01) is parsed via `Date.new-from-daycount()`.

    * Tag 1004 (date string) is parsed as a `Date`.

UNDEFINED VALUES
----------------

  * CBOR's `null` is translated as `Any` in Raku.

  * CBOR's `undefined` is translated as `Mu` in Raku.

  * A real `Nil` in an array (which must be *bound*, not assigned) is encoded as a CBOR Absent tag (31). Absent values will be recognized on decode as well, but since array contents are *assigned* into their parent array during decoding, a `Nil` in an array will be translated to `Any` by Raku's array assignment semantics.

OTHER SPECIAL CASES
-------------------

  * To mark a substructure for lazy decoding (treating it as an opaque `Blob` until explicitly decoded), use the tagged value idiom in the SYNOPSIS with `:tag-number(24)` (encoded CBOR value) or `:tag-number(63)` (encoded CBOR Sequence).

  * CBOR strings claiming to be longer than `2⁶³-1` are treated as malformed.

  * Bigfloats and decimal fractions (tags 4, 5, 264, 265) with very large exponents may result in numeric overflow when decoded.

  * Keys for Associative types are sorted using Raku's internal `sort` method rather than the RFC 8949 default sort, because the latter is much slower.

  * `cbor-diagnostic()` always adds encoding indicators for float values.

TAG IMPLEMENTATION STATUS
-------------------------

Note that unrecognized tags will decode to their contents wrapped with a `CBOR::Simple::Tagged` object that records its `tag-number`; check marks in the details table indicate conversion to/from an appropriate native Raku type rather than this default behavior.

<table class="pod-table">
<caption>Tag Status Overview: Native Raku Types</caption>
<thead><tr>
<th>GROUP</th> <th>SUPPORT</th> <th>NOTES</th>
</tr></thead>
<tbody>
<tr> <td>Core</td> <td>Good</td> <td>Core RFC 8949 CBOR data model and syntax</td> </tr> <tr> <td>Collections</td> <td>Good</td> <td>Sets, maps with only object or only string keys</td> </tr> <tr> <td>Graph</td> <td>NONE</td> <td>Cyclic, indirected, and self-referential structures</td> </tr> <tr> <td>Numbers</td> <td>Good</td> <td>Rational/BigInt/BigFloat support except non-finite triplets</td> </tr> <tr> <td>Packed Arrays</td> <td>Partial</td> <td>Packed num32/64, [u]int* arrays full; num16 is read-only</td> </tr> <tr> <td>Special Arrays</td> <td>NONE</td> <td>Explicit multi-dim/homogenous arrays</td> </tr> <tr> <td>Tag Fallbacks</td> <td>Good</td> <td>Round tripping of unknown tagged content</td> </tr> <tr> <td>Date/Time</td> <td>Partial</td> <td>All but tagged time (tags 1001-1003) supported</td> </tr>
</tbody>
</table>

<table class="pod-table">
<caption>Tag Status Overview: Specialty Types</caption>
<thead><tr>
<th>GROUP</th> <th>SUPPORT</th> <th>NOTES</th>
</tr></thead>
<tbody>
<tr> <td>Encodings</td> <td>NONE</td> <td>baseN, MIME, YANG, BER, non-UTF-8 strings</td> </tr> <tr> <td>Geo</td> <td>NONE</td> <td>Geographic coordinates and shapes</td> </tr> <tr> <td>Identifiers</td> <td>NONE</td> <td>URI, IRI, UUID, IPLD CID, general identifiers</td> </tr> <tr> <td>Networking</td> <td>NONE</td> <td>IPv4/IPv6 addresses, subnets, and masks</td> </tr> <tr> <td>Security</td> <td>NONE</td> <td>COSE and CWT</td> </tr> <tr> <td>Specialty</td> <td>NONE</td> <td>IoT data, Openswan, PlatformV, DOTS, ERIS, RAINS</td> </tr> <tr> <td>String Hints</td> <td>NONE</td> <td>JSON conversions, language tags, regex</td> </tr>
</tbody>
</table>

<table class="pod-table">
<caption>Tag Status Details</caption>
<thead><tr>
<th>SPEC</th> <th>TAGS</th> <th>ENCODE</th> <th>DECODE</th> <th>NOTES</th>
</tr></thead>
<tbody>
<tr> <td>RFC 8949</td> <td>0</td> <td>→</td> <td>✓</td> <td>DateTime strings → Encoded as tag 1</td> </tr> <tr> <td>RFC 8949</td> <td>1</td> <td>✓</td> <td>✓</td> <td>DateTime/Instant</td> </tr> <tr> <td>RFC 8949</td> <td>2,3</td> <td>✓</td> <td>✓</td> <td>(Big) Int</td> </tr> <tr> <td>RFC 8949</td> <td>4,5</td> <td>→</td> <td>✓</td> <td>Big fractions → Encoded as tag 30</td> </tr> <tr> <td>unassigned</td> <td>6-15</td> <td></td> <td></td> <td></td> </tr> <tr> <td>COSE</td> <td>16-18</td> <td>✘</td> <td>✘</td> <td>MAC/Signatures</td> </tr> <tr> <td>unassigned</td> <td>19-20</td> <td></td> <td></td> <td></td> </tr> <tr> <td>RFC 8949</td> <td>21-23</td> <td>✘</td> <td>✘</td> <td>Expected JSON conversion to baseN</td> </tr> <tr> <td>RFC 8949</td> <td>24</td> <td>T</td> <td>✓</td> <td>Encoded CBOR data item</td> </tr> <tr> <td>[Lehmann]</td> <td>25</td> <td>✘</td> <td>✘</td> <td>String backrefs</td> </tr> <tr> <td>[Lehmann]</td> <td>26,27</td> <td>✘</td> <td>✘</td> <td>General serialized objects</td> </tr> <tr> <td>[Lehmann]</td> <td>28,29</td> <td>✘</td> <td>✘</td> <td>Shareable referenced values</td> </tr> <tr> <td>[Occil]</td> <td>30</td> <td>✓</td> <td>✓</td> <td>Rational numbers</td> </tr> <tr> <td>[Vaarala]</td> <td>31</td> <td>✓</td> <td>*</td> <td>Absent values</td> </tr> <tr> <td>RFC 8949</td> <td>32-34</td> <td>✘</td> <td>✘</td> <td>URIs and base64 encoding</td> </tr> <tr> <td>RFC 7094</td> <td>35</td> <td>D</td> <td>D</td> <td>PCRE/ECMA 262 regex (DEPRECATED)</td> </tr> <tr> <td>RFC 8949</td> <td>36</td> <td>✘</td> <td>✘</td> <td>Text-based MIME message</td> </tr> <tr> <td>[Clemente]</td> <td>37</td> <td>✘</td> <td>✘</td> <td>Binary UUID</td> </tr> <tr> <td>[Occil]</td> <td>38</td> <td>✘</td> <td>✘</td> <td>Language-tagged string</td> </tr> <tr> <td>[Clemente]</td> <td>39</td> <td>✘</td> <td>✘</td> <td>Identifier semantics</td> </tr> <tr> <td>RFC 8746</td> <td>40</td> <td>✘</td> <td>✘</td> <td>Row-major multidim array</td> </tr> <tr> <td>RFC 8746</td> <td>41</td> <td>✘</td> <td>✘</td> <td>Homogenous array</td> </tr> <tr> <td>[Mische]</td> <td>42</td> <td>✘</td> <td>✘</td> <td>IPLD content identifier</td> </tr> <tr> <td>[YANG]</td> <td>43-47</td> <td>✘</td> <td>✘</td> <td>YANG datatypes</td> </tr> <tr> <td>unassigned</td> <td>48-51</td> <td></td> <td></td> <td></td> </tr> <tr> <td>draft</td> <td>52</td> <td>D</td> <td>D</td> <td>IPv4 address/network (DEPRECATED)</td> </tr> <tr> <td>unassigned</td> <td>53</td> <td></td> <td></td> <td></td> </tr> <tr> <td>draft</td> <td>54</td> <td>D</td> <td>D</td> <td>IPv6 address/network (DEPRECATED)</td> </tr> <tr> <td>unassigned</td> <td>55-60</td> <td></td> <td></td> <td></td> </tr> <tr> <td>RFC 8392</td> <td>61</td> <td>✘</td> <td>✘</td> <td>CBOR Web Token (CWT)</td> </tr> <tr> <td>unassigned</td> <td>62</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Bormann]</td> <td>63</td> <td>T</td> <td>✓</td> <td>Encoded CBOR Sequence</td> </tr> <tr> <td>RFC 8746</td> <td>64-79</td> <td>✓</td> <td>✓</td> <td>Packed int arrays</td> </tr> <tr> <td>RFC 8746</td> <td>80-87</td> <td>*</td> <td>✓</td> <td>Packed num arrays (except 128-bit)</td> </tr> <tr> <td>unassigned</td> <td>88-95</td> <td></td> <td></td> <td></td> </tr> <tr> <td>COSE</td> <td>96-98</td> <td>✘</td> <td>✘</td> <td>Encryption/MAC/Signatures</td> </tr> <tr> <td>unassigned</td> <td>99</td> <td></td> <td></td> <td></td> </tr> <tr> <td>RFC 8943</td> <td>100</td> <td>✓</td> <td>✓</td> <td>Date</td> </tr> <tr> <td>unassigned</td> <td>101-102</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Vidovic]</td> <td>103</td> <td>✘</td> <td>✘</td> <td>Geo coords</td> </tr> <tr> <td>[Clarke]</td> <td>104</td> <td>✘</td> <td>✘</td> <td>Geo coords ref system WKT/EPSG</td> </tr> <tr> <td>unassigned</td> <td>105-109</td> <td></td> <td></td> <td></td> </tr> <tr> <td>RFC 9090</td> <td>110-112</td> <td>✘</td> <td>✘</td> <td>BER-encoded object ID</td> </tr> <tr> <td>unassigned</td> <td>113-119</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Vidovic]</td> <td>120</td> <td>✘</td> <td>✘</td> <td>IoT data point</td> </tr> <tr> <td>unassigned</td> <td>121-255</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Lehmann]</td> <td>256</td> <td>✘</td> <td>✘</td> <td>String backrefs (see tag 25)</td> </tr> <tr> <td>[Occil]</td> <td>257</td> <td>✘</td> <td>✘</td> <td>Binary MIME message</td> </tr> <tr> <td>[Napoli]</td> <td>258</td> <td>✓</td> <td>✓</td> <td>Set</td> </tr> <tr> <td>[Holloway]</td> <td>259</td> <td>T</td> <td>✓</td> <td>Map with object keys</td> </tr> <tr> <td>[Raju]</td> <td>260-261</td> <td>✘</td> <td>✘</td> <td>IPv4/IPv6/MAC address/network</td> </tr> <tr> <td>[Raju]</td> <td>262-263</td> <td>✘</td> <td>✘</td> <td>Embedded JSON/hex strings</td> </tr> <tr> <td>[Occil]</td> <td>264-265</td> <td>→</td> <td>*</td> <td>Extended fractions -&gt; Encoded as tag 30</td> </tr> <tr> <td>[Occil]</td> <td>266-267</td> <td>✘</td> <td>✘</td> <td>IRI/IRI reference</td> </tr> <tr> <td>[Occil]</td> <td>268-270</td> <td>✘✘</td> <td>✘✘</td> <td>Triplet non-finite numerics</td> </tr> <tr> <td>RFC 9132</td> <td>271</td> <td>✘✘</td> <td>✘✘</td> <td>DDoS Open Threat Signaling (DOTS)</td> </tr> <tr> <td>[Vaarala]</td> <td>272-274</td> <td>✘</td> <td>✘</td> <td>Non-UTF-8 strings</td> </tr> <tr> <td>[Cormier]</td> <td>275</td> <td>T</td> <td>✓</td> <td>Map with only string keys</td> </tr> <tr> <td>[ERIS]</td> <td>276</td> <td>✘</td> <td>✘</td> <td>ERIS binary read capability</td> </tr> <tr> <td>[Meins]</td> <td>277-278</td> <td>✘</td> <td>✘</td> <td>Geo area shape/velocity</td> </tr> <tr> <td>unassigned</td> <td>279-1000</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Bormann]</td> <td>1001-1003</td> <td>✘</td> <td>✘</td> <td>Extended time representations</td> </tr> <tr> <td>RFC 8943</td> <td>1004</td> <td>→</td> <td>✓</td> <td>→ Encoded as tag 100</td> </tr> <tr> <td>unassigned</td> <td>1005-1039</td> <td></td> <td></td> <td></td> </tr> <tr> <td>RFC 8746</td> <td>1040</td> <td>✘</td> <td>✘</td> <td>Column-major multidim array</td> </tr> <tr> <td>unassigned</td> <td>1041-22097</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Lehmann]</td> <td>22098</td> <td>✘</td> <td>✘</td> <td>Hint for additional indirection</td> </tr> <tr> <td>unassigned</td> <td>22099-25440</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Broadwell]</td> <td>25441</td> <td>✓</td> <td>✓</td> <td>Capture: reference implementation</td> </tr> <tr> <td>unassigned</td> <td>25442-49999</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Tongzhou]</td> <td>50000-50011</td> <td>✘✘</td> <td>✘✘</td> <td>PlatformV</td> </tr> <tr> <td>unassigned</td> <td>50012-55798</td> <td></td> <td></td> <td></td> </tr> <tr> <td>RFC 8949</td> <td>55799</td> <td>✓</td> <td>✓</td> <td>Self-described CBOR</td> </tr> <tr> <td>[Richardson]</td> <td>55800</td> <td>✓</td> <td>✓</td> <td>Self-described CBOR Sequence</td> </tr> <tr> <td>unassigned</td> <td>55801-65534</td> <td></td> <td></td> <td></td> </tr> <tr> <td>invalid</td> <td>65535</td> <td></td> <td>✓</td> <td>Invalid tag detected</td> </tr> <tr> <td>unassigned</td> <td>65536-15309735</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Trammell]</td> <td>15309736</td> <td>✘✘</td> <td>✘✘</td> <td>RAINS message</td> </tr> <tr> <td>unassigned</td> <td>15309737-1330664269</td> <td></td> <td></td> <td></td> </tr> <tr> <td>[Hussain]</td> <td>1330664270</td> <td>✘✘</td> <td>✘✘</td> <td>CBOR-encoded Openswan config file</td> </tr> <tr> <td>unassigned</td> <td>1330664271-4294967294</td> <td></td> <td></td> <td></td> </tr> <tr> <td>invalid</td> <td>4294967295</td> <td></td> <td>✓</td> <td>Invalid tag detected</td> </tr> <tr> <td>unassigned</td> <td>...</td> <td></td> <td></td> <td></td> </tr> <tr> <td>invalid</td> <td>18446744073709551615</td> <td></td> <td>✓</td> <td>Invalid tag detected</td> </tr>
</tbody>
</table>

<table class="pod-table">
<caption>Tag Table Symbol Key</caption>
<thead><tr>
<th>SYMBOL</th> <th>MEANING</th>
</tr></thead>
<tbody>
<tr> <td>✓</td> <td>Fully supported</td> </tr> <tr> <td>*</td> <td>Supported, but see notes above</td> </tr> <tr> <td>T</td> <td>Encoding supported by explicitly tagging contents</td> </tr> <tr> <td>→</td> <td>Raku values will be encoded using a different tag</td> </tr> <tr> <td>D</td> <td>Deprecated and unsupported tag spec; may eventually be decodable</td> </tr> <tr> <td>✘</td> <td>Not yet implemented</td> </tr> <tr> <td>✘!</td> <td>Not yet implemented, but already requested</td> </tr> <tr> <td>✘?</td> <td>Not yet implemented, but may be easy to add</td> </tr> <tr> <td>✘✘</td> <td>Probably won&#39;t be implemented in CBOR::Simple</td> </tr>
</tbody>
</table>

AUTHOR
======

Geoffrey Broadwell <gjb@sonic.net>

COPYRIGHT AND LICENSE
=====================

Copyright 2021,2025 Geoffrey Broadwell

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

