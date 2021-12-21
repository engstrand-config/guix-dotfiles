(define-module (engstrand packages rust)
            #:use-module (gnu packages crates-io)
            #:use-module (guix download)
            #:use-module (guix packages)
            #:use-module (guix build-system cargo)
            #:use-module ((guix licenses) #:prefix license:))

(define-public rust-num-bigint-dig-0.7
  (package
    (name "rust-num-bigint-dig")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-bigint-dig" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1004mmipvc7pvaf3kf13i1nqh3vxf789bj72d8wl51y185aywis5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-0.1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libm" ,rust-libm-0.2)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-iter" ,rust-num-iter-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-serde" ,rust-serde-1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-zeroize" ,rust-zeroize-1))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.8)
         ("rust-rand-chacha" ,rust-rand-chacha-0.3)
         ("rust-rand-isaac" ,rust-rand-isaac-0.3)
         ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
         ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/dignifiedquire/num-bigint")
    (synopsis "Big integer implementation for Rust")
    (description "Big integer implementation for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-zeroize-1.4
  (package
    (name "rust-zeroize")
    (version "1.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zeroize" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "068nvl3n5hk6lfn5y24grf2c7anzzqfzjjccscq3md7rqp79v3fn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis
      "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (description
      "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-block-modes-0.8
  (package
    (name "rust-block-modes")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-modes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "13id7rw1lhi83i701za8w5is3a8qkf4vfigqw3f8jp8mxldkvc1c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-block-padding" ,rust-block-padding-0.2)
         ("rust-cipher" ,rust-cipher-0.3))
        #:cargo-development-inputs
        (("rust-aes" ,rust-aes-0.7)
         ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Block cipher modes of operation")
    (description "Block cipher modes of operation")
    (license (list license:expat license:asl2.0))))

(define-public rust-cpufeatures-0.2
  (package
    (name "rust-cpufeatures")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpufeatures" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sgllzsvs8hinylaiigmd9c908gd8wclxnqz8dinpxbdyql981cm"))))
    (build-system cargo-build-system)
    (arguments `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Lightweight and efficient no-std compatible alternative to the
is_x86_feature_detected! macro
")
    (description
      "Lightweight and efficient no-std compatible alternative to the
is_x86_feature_detected! macro
")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-0.7
  (package
    (name "rust-aes")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1f0sdx2fsa8w3l7xzsyi9ry3shvnnsgc0znh50if9fm95vslg2wy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-cipher" ,rust-cipher-0.3)
         ("rust-cpufeatures" ,rust-cpufeatures-0.2)
         ("rust-ctr" ,rust-ctr-0.6)
         ("rust-opaque-debug" ,rust-opaque-debug-0.3))
        #:cargo-development-inputs
        (("rust-cipher" ,rust-cipher-0.3)
         ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis
      "Pure Rust implementation of the Advanced Encryption Standard (a.k.a. Rijndael)
including support for AES in counter mode (a.k.a. AES-CTR)
")
    (description
      "Pure Rust implementation of the Advanced Encryption Standard (a.k.a.  Rijndael)
including support for AES in counter mode (a.k.a.  AES-CTR)
")
    (license (list license:expat license:asl2.0))))

(define-public rust-primitive-types-0.10
  (package
    (name "rust-primitive-types")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "primitive-types" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wr3yhpc59m5xbhqs69j8qm0hz0xh8q8r806bnfsjn3sd4n75r05"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fixed-hash" ,rust-fixed-hash-0.7)
         ("rust-impl-codec" ,rust-impl-codec-0.5)
         ("rust-impl-num-traits" ,rust-impl-num-traits-0.1)
         ("rust-impl-rlp" ,rust-impl-rlp-0.3)
         ("rust-impl-serde" ,rust-impl-serde-0.3)
         ("rust-scale-info" ,rust-scale-info-1)
         ("rust-uint" ,rust-uint-0.9))))
    (home-page "https://github.com/paritytech/parity-common")
    (synopsis "Primitive types shared by Ethereum and Substrate")
    (description "Primitive types shared by Ethereum and Substrate")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-hex-2
  (package
    (name "rust-rustc-hex")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-hex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1mkjy2vbn5kzg67wgngwddlk4snmd8mkjkql2dzrzzfh6ajzcx9y"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/debris/rustc-hex")
    (synopsis "rustc-serialize compatible hex conversion traits
")
    (description "rustc-serialize compatible hex conversion traits
")
    (license (list license:expat license:asl2.0))))

(define-public rust-rlp-0.5
  (package
    (name "rust-rlp")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rlp" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1da7b1hc4czlmsyr7ifs9bz9fv8hi5dw8q14xnmjlydfn2mhi5cr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes-1) ("rust-rustc-hex" ,rust-rustc-hex-2))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.3)
         ("rust-hex-literal" ,rust-hex-literal-0.3)
         ("rust-primitive-types" ,rust-primitive-types-0.10))))
    (home-page "https://github.com/paritytech/parity-common")
    (synopsis "Recursive-length prefix encoding, decoding, and compression")
    (description "Recursive-length prefix encoding, decoding, and compression")
    (license (list license:expat license:asl2.0))))

(define-public rust-password-hash-0.3
  (package
    (name "rust-password-hash")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "password-hash" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1n7ig9j5x2q0fk12nny40faggrs0ra5bbxp6gz5yghfwlqw1ay8x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64ct" ,rust-base64ct-1.0.1)
         ("rust-rand-core" ,rust-rand-core-0.6)
         ("rust-subtle" ,rust-subtle-2))))
    (home-page
      "https://github.com/RustCrypto/traits/tree/master/password-hash")
    (synopsis
      "Traits which describe the functionality of password hashing algorithms,
as well as a `no_std`-friendly implementation of the PHC string format
(a well-defined subset of the Modular Crypt Format a.k.a. MCF)
")
    (description
      "Traits which describe the functionality of password hashing algorithms,
as well as a `no_std`-friendly implementation of the PHC string format
(a well-defined subset of the Modular Crypt Format a.k.a.  MCF)
")
    (license (list license:expat license:asl2.0))))

(define-public rust-der-derive-0.4
  (package
    (name "rust-der-derive")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "der-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0snv85yfy9iln05qsgbhwr1159gd0jfrgzj5dkrnricdc0y3pvca"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1)
         ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der/derive")
    (synopsis
      "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (description
      "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (license (list license:asl2.0 license:expat))))

(define-public rust-subtle-2.4
  (package
    (name "rust-subtle")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "subtle" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "00b6jzh9gzb0h9n25g06nqr90z3xzqppfhhb260s1hjhh4pg7pkb"))))
    (build-system cargo-build-system)
    (arguments `(#:cargo-development-inputs (("rust-rand" ,rust-rand-0.7))))
    (home-page "https://dalek.rs/")
    (synopsis
      "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (description
      "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-crypto-bigint-0.2
  (package
    (name "rust-crypto-bigint")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-bigint" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "00qckh65nzb7s7vd60wylw6alxf9g37xh31lirb1qw0l8fxx6fzq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-generic-array" ,rust-generic-array-0.14)
         ("rust-rand-core" ,rust-rand-core-0.6)
         ("rust-rlp" ,rust-rlp-0.5)
         ("rust-subtle" ,rust-subtle-2.4)
         ("rust-zeroize" ,rust-zeroize-1.4))
        #:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.3)
         ("rust-num-bigint" ,rust-num-bigint-0.4)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-proptest" ,rust-proptest-1)
         ("rust-rand-chacha" ,rust-rand-chacha-0.3))))
    (home-page "https://github.com/RustCrypto/crypto-bigint")
    (synopsis
      "Pure Rust implementation of a big integer library which has been designed from
the ground-up for use in cryptographic applications. Provides constant-time,
no_std-friendly implementations of modern formulas using const generics.
")
    (description
      "Pure Rust implementation of a big integer library which has been designed from
the ground-up for use in cryptographic applications.  Provides constant-time,
no_std-friendly implementations of modern formulas using const generics.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-const-oid-0.6
  (package
    (name "rust-const-oid")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const-oid" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "12vv7csqqjj0x1l5mf51lgqiw76k5c3mb1yzfhfcqysks2j2lvwx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/const-oid")
    (synopsis
      "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard
as defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e. embedded) support
")
    (description
      "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard
as defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e.  embedded) support
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-base64ct-1.0.1
  (package
    (name "rust-base64ct")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64ct" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sx4a44c2n450lsmi0q1mgfbjhkw1sx57462cv77p0mmy9mgscla"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustCrypto/formats/tree/master/base64ct")
    (synopsis
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support
")
    (description
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-base64ct-1
  (package
    (name "rust-base64ct")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64ct" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0p4was874qc90q2chm2i14m9mn8zmxjis8vaxihd6a2x4aqxkd76"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustCrypto/formats/tree/master/base64ct")
    (synopsis
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support
")
    (description
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pbkdf2-0.9
  (package
    (name "rust-pbkdf2")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pbkdf2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0fa7j0gdgghk64qlhzdv32yg52p0cfaz5ifhk7i4pfm1wsy98n7h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crypto-mac" ,rust-crypto-mac-0.11)
         ("rust-hmac" ,rust-hmac-0.11)
         ("rust-password-hash" ,rust-password-hash-0.3)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9))
        #:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.3)
         ("rust-hmac" ,rust-hmac-0.11)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-streebog" ,rust-streebog-0.9))))
    (home-page
      "https://github.com/RustCrypto/password-hashes/tree/master/pbkdf2")
    (synopsis "Generic implementation of PBKDF2")
    (description "Generic implementation of PBKDF2")
    (license (list license:expat license:asl2.0))))

(define-public rust-spki-0.4
  (package
    (name "rust-spki")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spki" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ckgkcg6db5y94dqhmyikgn8yrsah6pyf4j197hv1c51bp0s00aw"))))
    (build-system cargo-build-system)
    (arguments `(#:cargo-inputs (("rust-der" ,rust-der-0.4))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/spki")
    (synopsis
      "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their
associated AlgorithmIdentifiers (i.e. OIDs)
")
    (description
      "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their
associated AlgorithmIdentifiers (i.e.  OIDs)
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs5-0.3
  (package
    (name "rust-pkcs5")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m3xrrwwbn9883bylgjzssfh3w1lbl7fhkb3ndz721rf27pca8sl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-aes" ,rust-aes-0.7)
         ("rust-block-modes" ,rust-block-modes-0.8)
         ("rust-der" ,rust-der-0.4)
         ("rust-des" ,rust-des-0.6)
         ("rust-hmac" ,rust-hmac-0.11)
         ("rust-pbkdf2" ,rust-pbkdf2-0.9)
         ("rust-scrypt" ,rust-scrypt-0.3)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-spki" ,rust-spki-0.4))
        #:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs5")
    (synopsis
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #5:
Password-Based Cryptography Specification Version 2.1 (RFC 8018)
")
    (description
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #5:
Password-Based Cryptography Specification Version 2.1 (RFC 8018)
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pem-rfc7468-0.2
  (package
    (name "rust-pem-rfc7468")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pem-rfc7468" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m1c9jypydzabg4yscplmvff7pdcc8gg4cqg081hnlf03hxkmsc4"))))
    (build-system cargo-build-system)
    (arguments `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pem-rfc7468")
    (synopsis
      "PEM Encoding (RFC 7468) for PKIX, PKCS, and CMS Structures, implementing a
strict subset of the original Privacy-Enhanced Mail encoding intended
specifically for use with cryptographic keys, certificates, and other messages.
Provides a no_std-friendly, constant-time implementation suitable for use with
cryptographic private keys.
")
    (description
      "PEM Encoding (RFC 7468) for PKIX, PKCS, and CMS Structures, implementing a
strict subset of the original Privacy-Enhanced Mail encoding intended
specifically for use with cryptographic keys, certificates, and other messages.
Provides a no_std-friendly, constant-time implementation suitable for use with
cryptographic private keys.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-der-0.4
  (package
    (name "rust-der")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "der" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1x4k0jln8va1657cghl40l6p7hyvr1ixz71v9cd6imwmgp51rdvr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-const-oid" ,rust-const-oid-0.6)
         ("rust-crypto-bigint" ,rust-crypto-bigint-0.2)
         ("rust-der-derive" ,rust-der-derive-0.4))
        #:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der")
    (synopsis
      "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets
")
    (description
      "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs8-0.7
  (package
    (name "rust-pkcs8")
    (version "0.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs8" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0iq46p6fa2b8xy6pj52zpmdy8ya3fg31dj4rc19x1fi69nvgjgpf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-der" ,rust-der-0.4)
         ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.2)
         ("rust-pkcs1" ,rust-pkcs1-0.2)
         ("rust-pkcs5" ,rust-pkcs5-0.3)
         ("rust-rand-core" ,rust-rand-core-0.6)
         ("rust-spki" ,rust-spki-0.4)
         ("rust-zeroize" ,rust-zeroize-1.4))
        #:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs8")
    (synopsis
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #8:
Private-Key Information Syntax Specification (RFC 5208), with additional
support for PKCS#8v2 asymmetric key packages (RFC 5958)
")
    (description
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #8:
Private-Key Information Syntax Specification (RFC 5208), with additional
support for PKCS#8v2 asymmetric key packages (RFC 5958)
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs1-0.2
  (package
    (name "rust-pkcs1")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs1" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0b2f1a0lf5h53zrjvcqbxzjhh89gcfa1myhf6z7w10ypg61fwsqi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-der" ,rust-der-0.4)
         ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.2)
         ("rust-zeroize" ,rust-zeroize-1.4))
        #:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs1")
    (synopsis
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #1:
RSA Cryptography Specifications Version 2.2 (RFC 8017)
")
    (description
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #1:
RSA Cryptography Specifications Version 2.2 (RFC 8017)
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-paw-structopt-1
  (package
    (name "rust-paw-structopt")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paw-structopt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1iwg83xqjpfgpy8wrq173cy7zgkyxfryd230sh34f5qsjdx7zap4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-0.4)
         ("rust-quote" ,rust-quote-0.6)
         ("rust-structopt" ,rust-structopt-0.2)
         ("rust-syn" ,rust-syn-0.15))))
    (home-page "https://github.com/rust-cli/paw")
    (synopsis "Structopt support for the Paw crate.")
    (description "Structopt support for the Paw crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-paw-raw-1
  (package
    (name "rust-paw-raw")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paw-raw" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1wk76ipp34gjh42vivmgdkb2rgr26gwhn34gk7z5l378ixk5j2vz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-cli/paw")
    (synopsis "Traits to implement custom Paw implementations")
    (description "Traits to implement custom Paw implementations")
    (license (list license:expat license:asl2.0))))

(define-public rust-paw-attributes-1
  (package
    (name "rust-paw-attributes")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paw-attributes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0fda1v7y5pfmg8d2v7m0pyvif6c44qjz914jjn718pdyclrmhd8g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rust-cli/paw")
    (synopsis "Proc Macro attributes for the Paw crate.")
    (description "Proc Macro attributes for the Paw crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-totp-lite-1
  (package
    (name "rust-totp-lite")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "totp-lite" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "12ql4pi9q7sf5651588wia2l5h4mil3kv9jrrkib5gvlpvl0k05i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-digest" ,rust-digest-0.9)
         ("rust-hmac" ,rust-hmac-0.11)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9))
        #:cargo-development-inputs
        (("rust-version-sync" ,rust-version-sync-0.8))))
    (home-page "https://github.com/fosskers/totp-lite")
    (synopsis "A simple, correct TOTP library.")
    (description "This package provides a simple, correct TOTP library.")
    (license license:expat)))

(define-public rust-rsa-0.5
  (package
    (name "rust-rsa")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rsa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "039676a4mj0875phdi7vc0bd37hv84dh0dql6fmk8dl2w81jcp70"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-digest" ,rust-digest-0.9)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.7)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-iter" ,rust-num-iter-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-pkcs1" ,rust-pkcs1-0.2)
         ("rust-pkcs8" ,rust-pkcs8-0.7)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-serde" ,rust-serde-1)
         ("rust-subtle" ,rust-subtle-2)
         ("rust-zeroize" ,rust-zeroize-1.4))
        #:cargo-development-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-hex" ,rust-hex-0.4)
         ("rust-hex-literal" ,rust-hex-literal-0.3)
         ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
         ("rust-serde-test" ,rust-serde-test-1)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-sha3" ,rust-sha3-0.9))))
    (home-page "https://github.com/RustCrypto/RSA")
    (synopsis "Pure Rust RSA implementation")
    (description "Pure Rust RSA implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-rsa-0.5
  (package
    (name "rust-rsa")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rsa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "039676a4mj0875phdi7vc0bd37hv84dh0dql6fmk8dl2w81jcp70"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-digest" ,rust-digest-0.9)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.7)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-iter" ,rust-num-iter-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-pkcs1" ,rust-pkcs1-0.2)
         ("rust-pkcs8" ,rust-pkcs8-0.7)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-serde" ,rust-serde-1)
         ("rust-subtle" ,rust-subtle-2)
         ("rust-zeroize" ,rust-zeroize-1.4))
        #:cargo-development-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-hex" ,rust-hex-0.4)
         ("rust-hex-literal" ,rust-hex-literal-0.3)
         ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
         ("rust-serde-test" ,rust-serde-test-1)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-sha3" ,rust-sha3-0.9))))
    (home-page "https://github.com/RustCrypto/RSA")
    (synopsis "Pure Rust RSA implementation")
    (description "Pure Rust RSA implementation")
    (license (list license:expat license:asl2.0))))
(define-public rust-paw-1
  (package
    (name "rust-paw")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paw" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1sc481y42rb08hmww525m4539ppl8k0w14kwxp13vg2dasdzrh09"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-paw-attributes" ,rust-paw-attributes-1)
         ("rust-paw-raw" ,rust-paw-raw-1))
        #:cargo-development-inputs
        (("rust-paw-structopt" ,rust-paw-structopt-1)
         ("rust-runtime" ,rust-runtime-0.3)
         ("rust-structopt" ,rust-structopt-0.2))))
    (home-page "https://github.com/rust-cli/paw")
    (synopsis "CLI argument parser.")
    (description "CLI argument parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mmap-fixed-0.1
  (package
    (name "rust-mmap-fixed")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mmap-fixed" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08bvs08wxijkfx635cbc2324s1vdksygcjcm0ysd6hv39lkaxh97"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-winapi" ,rust-winapi-0.2))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "")
    (synopsis
      "A library for dealing with memory-mapped I/O

This is a fork of the original rust-mmap with updated dependencies and a
fix for the Windows version. This exists only because there are no other
alternative crates for `MAP_FIXED` allocations.
")
    (description
      "This package provides a library for dealing with memory-mapped I/O

This is a fork of the original rust-mmap with updated dependencies and a
fix for the Windows version.  This exists only because there are no other
alternative crates for `MAP_FIXED` allocations.
")
    (license license:expat)))

(define-public rust-region-3
  (package
    (name "rust-region")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "region" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0bji1p0c9abzh78ps5hs0ygg9pxkg7gjspll43lxr14q6v18kqbn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-mach" ,rust-mach-0.3)
         ("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-mmap-fixed" ,rust-mmap-fixed-0.1))))
    (home-page "https://github.com/darfink/region-rs")
    (synopsis "Cross-platform virtual memory API")
    (description "Cross-platform virtual memory API")
    (license license:expat)))

(define-public rust-libc-0.2.102
  (package
    (name "rust-libc")
    (version "0.2.102")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "00z1f4hypdkbvajk6rj9yqjk7k4acgg5yzi64flg7z2bk27sr9d2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-nix-0.23
  (package
    (name "rust-nix")
    (version "0.23.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nix" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1iimixk7y2qk0jswqich4mkd8kqyzdghcgy6203j8fmxmhbn71lz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-cc" ,rust-cc-1)
         ("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-libc" ,rust-libc-0.2.102)
         ("rust-memoffset" ,rust-memoffset-0.6))
        #:cargo-development-inputs
        (("rust-assert-impl" ,rust-assert-impl-0.1)
         ("rust-caps" ,rust-caps-0.5)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-semver" ,rust-semver-1)
         ("rust-sysctl" ,rust-sysctl-0.1)
         ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "Rust friendly bindings to *nix APIs")
    (license license:expat)))

(define-public rust-hkdf-0.11
  (package
    (name "rust-hkdf")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hkdf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sw8bz79xqq3bc5dh6nzv084g7va13j3lrqf91c10a2wimbnsw01"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-digest" ,rust-digest-0.9) ("rust-hmac" ,rust-hmac-0.11))
        #:cargo-development-inputs
        (("rust-bencher" ,rust-bencher-0.1)
         ("rust-blobby" ,rust-blobby-0.3)
         ("rust-crypto-tests" ,rust-crypto-tests-0.5)
         ("rust-hex" ,rust-hex-0.4)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
      "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-humantime-2.1
  (package
    (name "rust-humantime")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "humantime" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-chrono" ,rust-chrono-0.4)
         ("rust-rand" ,rust-rand-0.6)
         ("rust-time" ,rust-time-0.1))))
    (home-page "https://github.com/tailhook/humantime")
    (synopsis
      "    A parser and formatter for std::time::{Duration, SystemTime}
")
    (description
      "    A parser and formatter for std::time::{Duration, SystemTime}
")
    (license (list license:expat license:asl2.0))))

(define-public rust-env-logger-0.9
  (package
    (name "rust-env-logger")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env-logger" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hzr53r0wga51j0w5zid69ylbfizg4qdbq0vqdj6rvki94sg0b0b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-atty" ,rust-atty-0.2)
         ("rust-humantime" ,rust-humantime-2.1)
         ("rust-log" ,rust-log-0.4)
         ("rust-regex" ,rust-regex-1)
         ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/env-logger-rs/env_logger/")
    (synopsis
      "A logging implementation for `log` which is configured via an environment
variable.
")
    (description
      "This package provides a logging implementation for `log` which is configured via an environment
variable.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-argon2-0.8
  (package
    (name "rust-rust-argon2")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust-argon2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1yvqkv04fqk3cbvyasibr4bqbxa6mij8jdvibakwlcsbjh6q462b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-blake2b-simd" ,rust-blake2b-simd-0.5)
         ("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
         ("rust-serde" ,rust-serde-1))
        #:cargo-development-inputs
        (("rust-hex" ,rust-hex-0.4))))
    (home-page "https://github.com/sru-systems/rust-argon2")
    (synopsis "Rust implementation of the Argon2 password hashing function.")
    (description
      "Rust implementation of the Argon2 password hashing function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-redox-users-0.4
  (package
    (name "rust-redox-users")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox-users" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0r5y1a26flkn6gkayi558jg5dzh2m2fdsapgkpn7mj01v3rk51aj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getrandom" ,rust-getrandom-0.2)
         ("rust-redox-syscall" ,rust-redox-syscall-0.2)
         ("rust-rust-argon2" ,rust-rust-argon2-0.8))))
    (home-page "https://gitlab.redox-os.org/redox-os/users")
    (synopsis "A Rust library to access Redox users and groups functionality")
    (description
      "This package provides a Rust library to access Redox users and groups functionality")
    (license license:expat)))

(define-public rust-dirs-sys-0.3.6
  (package
    (name "rust-dirs-sys")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "102pbpcrfhvhfyfnyvmvvwpl6mfvynh170f6ima6fyinxls6bn03"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-redox-users" ,rust-redox-users-0.4)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/dirs-dev/dirs-sys-rs")
    (synopsis
      "System-level helper functions for the dirs and directories crates.")
    (description
      "System-level helper functions for the dirs and directories crates.")
    (license (list license:expat license:asl2.0))))

(define-public rust-directories-4
  (package
    (name "rust-directories")
    (version "4.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "directories" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "045jbj5y2f1fmjs9rfcw95y0vjydb2rqqhz1sdnqhdmxv96ms77m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-dirs-sys" ,rust-dirs-sys-0.3.6))
        #:cargo-development-inputs
        (("rust-bencher" ,rust-bencher-0.1))))
    (home-page "https://github.com/soc/directories-rs")
    (synopsis
      "A tiny mid-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows and macOS by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
      "This package provides a tiny mid-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows and macOS by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mio-aio-0.6
  (package
    (name "rust-mio-aio")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio-aio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1z7s0swv1pgvzmn8gaj7cdgid75y3bcklcyqc2b9ihsvxpc6wcca"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-mio" ,rust-mio-0.7) ("rust-nix" ,rust-nix-0.22))
        #:cargo-development-inputs
        (("rust-assert-impl" ,rust-assert-impl-0.1)
         ("rust-log" ,rust-log-0.3)
         ("rust-mio" ,rust-mio-0.7)
         ("rust-sysctl" ,rust-sysctl-0.1)
         ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/asomers/mio-aio")
    (synopsis "POSIX AIO bindings for mio
")
    (description "POSIX AIO bindings for mio
")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustversion-1
  (package
    (name "rust-rustversion")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustversion" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gxj6skypbk0wlbks3pdqb0lclpwbzmyv9xbqkijsvk6zbl3ik7j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs (("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/rustversion")
    (synopsis "Conditional compilation according to rustc compiler version")
    (description "Conditional compilation according to rustc compiler version")
    (license (list license:expat license:asl2.0))))

(define-public rust-generator-0.7
  (package
    (name "rust-generator")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generator" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1vhj3f0rf4mlh5vz7pz5rxmgry1cc62x21mf9ld1r292m2f2gnf1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-rustversion" ,rust-rustversion-1)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Xudong-Huang/generator-rs.git")
    (synopsis "Stackfull Generator Library in Rust")
    (description "Stackfull Generator Library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-loom-0.5
  (package
    (name "rust-loom")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "loom" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02a30cv9l2afjq5bg42hgcjspx8fgwyij0cf9saw8b73539wgigd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-generator" ,rust-generator-0.7)
         ("rust-pin-utils" ,rust-pin-utils-0.1)
         ("rust-scoped-tls" ,rust-scoped-tls-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
        #:cargo-development-inputs
        (("rust-futures-util" ,rust-futures-util-0.3))))
    (home-page "https://github.com/tokio-rs/loom")
    (synopsis "Permutation testing for concurrent code")
    (description "Permutation testing for concurrent code")
    (license license:expat)))

(define-public rust-tokio-1.12
  (package
    (name "rust-tokio")
    (version "1.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1k7g4hyvbibjdvx9y5ppv1hsycx63vg2fiabwhx49a6wvrpl3hn2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-mio" ,rust-mio-0.7)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
         ("rust-tokio-macros" ,rust-tokio-macros-1)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-async-stream" ,rust-async-stream-0.3)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-loom" ,rust-loom-0.5)
         ("rust-mio-aio" ,rust-mio-aio-0.6)
         ("rust-mockall" ,rust-mockall-0.10)
         ("rust-nix" ,rust-nix-0.22)
         ("rust-ntapi" ,rust-ntapi-0.3)
         ("rust-proptest" ,rust-proptest-1)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-socket2" ,rust-socket2-0.4)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-tokio-stream" ,rust-tokio-stream-0.1)
         ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://tokio.rs")
    (synopsis
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (license license:expat)))
