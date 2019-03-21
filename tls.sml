functor IntAsBytes(structure I : INTEGER
                   structure W : WORD
                   val size : int) = struct
  type vector = I.int
  type elem = Word8Vector.elem

  fun sub (int, i) =
        let
          val () = if i >= size then raise Subscript else ()
          val w = W.fromLargeInt (I.toLarge int)
          val shifted = W.>> (w, 0w8 * Word.fromInt (size - 1 - i))
          val mask = W.fromLarge (Word.toLarge 0wxff)
          val masked = W.andb (shifted, mask)
        in
          Word8.fromLarge (W.toLarge masked)
        end

  fun tabulate (n, f : int -> elem) =
        let
          fun loop (i, acc) =
                if i >= n then W.toInt acc
                else
                  let
                    val acc' = W.orb (W.<< (acc, 0w8), W.fromLarge (Word8.toLarge (f i)))
                  in
                    loop (i + 1, acc')
                  end
        in
          loop (0, W.fromInt 0)
        end
end

structure IntAsBytes16 = IntAsBytes(structure I = Int
                                    structure W = Word
                                    val size = 2)
structure IntAsBytes32 = IntAsBytes(structure I = Int
                                    structure W = Word
                                    val size = 4)
structure LargeIntAsBytes32 = IntAsBytes(structure I = LargeInt
                                         structure W = LargeWord
                                         val size = 4)

structure CipherSuite = struct

  datatype cipher_suite_identifier = TLS_NULL_WITH_NULL_NULL              

                                   | TLS_RSA_WITH_NULL_MD5
                                   | TLS_RSA_WITH_NULL_SHA
                                   | TLS_RSA_WITH_NULL_SHA256
                                   | TLS_RSA_WITH_RC4_128_MD5
                                   | TLS_RSA_WITH_RC4_128_SHA
                                   | TLS_RSA_WITH_3DES_EDE_CBC_SHA
                                   | TLS_RSA_WITH_AES_128_CBC_SHA
                                   | TLS_RSA_WITH_AES_256_CBC_SHA
                                   | TLS_RSA_WITH_AES_128_CBC_SHA256
                                   | TLS_RSA_WITH_AES_256_CBC_SHA256

                                   | TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA
                                   | TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA
                                   | TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA
                                   | TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA
                                   | TLS_DH_DSS_WITH_AES_128_CBC_SHA
                                   | TLS_DH_RSA_WITH_AES_128_CBC_SHA
                                   | TLS_DHE_DSS_WITH_AES_128_CBC_SHA
                                   | TLS_DHE_RSA_WITH_AES_128_CBC_SHA
                                   | TLS_DH_DSS_WITH_AES_256_CBC_SHA
                                   | TLS_DH_RSA_WITH_AES_256_CBC_SHA
                                   | TLS_DHE_DSS_WITH_AES_256_CBC_SHA
                                   | TLS_DHE_RSA_WITH_AES_256_CBC_SHA
                                   | TLS_DH_DSS_WITH_AES_128_CBC_SHA256
                                   | TLS_DH_RSA_WITH_AES_128_CBC_SHA256
                                   | TLS_DHE_DSS_WITH_AES_128_CBC_SHA256
                                   | TLS_DHE_RSA_WITH_AES_128_CBC_SHA256
                                   | TLS_DH_DSS_WITH_AES_256_CBC_SHA256
                                   | TLS_DH_RSA_WITH_AES_256_CBC_SHA256
                                   | TLS_DHE_DSS_WITH_AES_256_CBC_SHA256
                                   | TLS_DHE_RSA_WITH_AES_256_CBC_SHA256

                                   | TLS_DH_anon_WITH_RC4_128_MD5
                                   | TLS_DH_anon_WITH_3DES_EDE_CBC_SHA
                                   | TLS_DH_anon_WITH_AES_128_CBC_SHA
                                   | TLS_DH_anon_WITH_AES_256_CBC_SHA
                                   | TLS_DH_anon_WITH_AES_128_CBC_SHA256
                                   | TLS_DH_anon_WITH_AES_256_CBC_SHA256

  fun toInt TLS_NULL_WITH_NULL_NULL             = 0x0000

    | toInt TLS_RSA_WITH_NULL_MD5               = 0x0001
    | toInt TLS_RSA_WITH_NULL_SHA               = 0x0002
    | toInt TLS_RSA_WITH_NULL_SHA256            = 0x003B
    | toInt TLS_RSA_WITH_RC4_128_MD5            = 0x0004
    | toInt TLS_RSA_WITH_RC4_128_SHA            = 0x0005
    | toInt TLS_RSA_WITH_3DES_EDE_CBC_SHA       = 0x000A
    | toInt TLS_RSA_WITH_AES_128_CBC_SHA        = 0x002F
    | toInt TLS_RSA_WITH_AES_256_CBC_SHA        = 0x0035
    | toInt TLS_RSA_WITH_AES_128_CBC_SHA256     = 0x003C
    | toInt TLS_RSA_WITH_AES_256_CBC_SHA256     = 0x003D

    | toInt TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA    = 0x000D
    | toInt TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA    = 0x0010
    | toInt TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA   = 0x0013
    | toInt TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA   = 0x0016
    | toInt TLS_DH_DSS_WITH_AES_128_CBC_SHA     = 0x0030
    | toInt TLS_DH_RSA_WITH_AES_128_CBC_SHA     = 0x0031
    | toInt TLS_DHE_DSS_WITH_AES_128_CBC_SHA    = 0x0032
    | toInt TLS_DHE_RSA_WITH_AES_128_CBC_SHA    = 0x0033
    | toInt TLS_DH_DSS_WITH_AES_256_CBC_SHA     = 0x0036
    | toInt TLS_DH_RSA_WITH_AES_256_CBC_SHA     = 0x0037
    | toInt TLS_DHE_DSS_WITH_AES_256_CBC_SHA    = 0x0038
    | toInt TLS_DHE_RSA_WITH_AES_256_CBC_SHA    = 0x0039
    | toInt TLS_DH_DSS_WITH_AES_128_CBC_SHA256  = 0x003E
    | toInt TLS_DH_RSA_WITH_AES_128_CBC_SHA256  = 0x003F
    | toInt TLS_DHE_DSS_WITH_AES_128_CBC_SHA256 = 0x0040
    | toInt TLS_DHE_RSA_WITH_AES_128_CBC_SHA256 = 0x0067
    | toInt TLS_DH_DSS_WITH_AES_256_CBC_SHA256  = 0x0068
    | toInt TLS_DH_RSA_WITH_AES_256_CBC_SHA256  = 0x0069
    | toInt TLS_DHE_DSS_WITH_AES_256_CBC_SHA256 = 0x006A
    | toInt TLS_DHE_RSA_WITH_AES_256_CBC_SHA256 = 0x006B

    | toInt TLS_DH_anon_WITH_RC4_128_MD5        = 0x0018
    | toInt TLS_DH_anon_WITH_3DES_EDE_CBC_SHA   = 0x001B
    | toInt TLS_DH_anon_WITH_AES_128_CBC_SHA    = 0x0034
    | toInt TLS_DH_anon_WITH_AES_256_CBC_SHA    = 0x003A
    | toInt TLS_DH_anon_WITH_AES_128_CBC_SHA256 = 0x006C
    | toInt TLS_DH_anon_WITH_AES_256_CBC_SHA256 = 0x006D

  fun fromInt 0x0000 = TLS_NULL_WITH_NULL_NULL

    | fromInt 0x0001 = TLS_RSA_WITH_NULL_MD5
    | fromInt 0x0002 = TLS_RSA_WITH_NULL_SHA
    | fromInt 0x003B = TLS_RSA_WITH_NULL_SHA256
    | fromInt 0x0004 = TLS_RSA_WITH_RC4_128_MD5
    | fromInt 0x0005 = TLS_RSA_WITH_RC4_128_SHA
    | fromInt 0x000A = TLS_RSA_WITH_3DES_EDE_CBC_SHA
    | fromInt 0x002F = TLS_RSA_WITH_AES_128_CBC_SHA
    | fromInt 0x0035 = TLS_RSA_WITH_AES_256_CBC_SHA
    | fromInt 0x003C = TLS_RSA_WITH_AES_128_CBC_SHA256
    | fromInt 0x003D = TLS_RSA_WITH_AES_256_CBC_SHA256

    | fromInt 0x000D = TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA
    | fromInt 0x0010 = TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA
    | fromInt 0x0013 = TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA
    | fromInt 0x0016 = TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA
    | fromInt 0x0030 = TLS_DH_DSS_WITH_AES_128_CBC_SHA
    | fromInt 0x0031 = TLS_DH_RSA_WITH_AES_128_CBC_SHA
    | fromInt 0x0032 = TLS_DHE_DSS_WITH_AES_128_CBC_SHA
    | fromInt 0x0033 = TLS_DHE_RSA_WITH_AES_128_CBC_SHA
    | fromInt 0x0036 = TLS_DH_DSS_WITH_AES_256_CBC_SHA
    | fromInt 0x0037 = TLS_DH_RSA_WITH_AES_256_CBC_SHA
    | fromInt 0x0038 = TLS_DHE_DSS_WITH_AES_256_CBC_SHA
    | fromInt 0x0039 = TLS_DHE_RSA_WITH_AES_256_CBC_SHA
    | fromInt 0x003E = TLS_DH_DSS_WITH_AES_128_CBC_SHA256
    | fromInt 0x003F = TLS_DH_RSA_WITH_AES_128_CBC_SHA256
    | fromInt 0x0040 = TLS_DHE_DSS_WITH_AES_128_CBC_SHA256
    | fromInt 0x0067 = TLS_DHE_RSA_WITH_AES_128_CBC_SHA256
    | fromInt 0x0068 = TLS_DH_DSS_WITH_AES_256_CBC_SHA256
    | fromInt 0x0069 = TLS_DH_RSA_WITH_AES_256_CBC_SHA256
    | fromInt 0x006A = TLS_DHE_DSS_WITH_AES_256_CBC_SHA256
    | fromInt 0x006B = TLS_DHE_RSA_WITH_AES_256_CBC_SHA256

    | fromInt 0x0018 = TLS_DH_anon_WITH_RC4_128_MD5
    | fromInt 0x001B = TLS_DH_anon_WITH_3DES_EDE_CBC_SHA
    | fromInt 0x0034 = TLS_DH_anon_WITH_AES_128_CBC_SHA
    | fromInt 0x003A = TLS_DH_anon_WITH_AES_256_CBC_SHA
    | fromInt 0x006C = TLS_DH_anon_WITH_AES_128_CBC_SHA256
    | fromInt 0x006D = TLS_DH_anon_WITH_AES_256_CBC_SHA256

    | fromInt _ = raise Fail ""

  fun toString TLS_NULL_WITH_NULL_NULL             = "TLS_NULL_WITH_NULL_NULL"

    | toString TLS_RSA_WITH_NULL_MD5               = "TLS_RSA_WITH_NULL_MD5"
    | toString TLS_RSA_WITH_NULL_SHA               = "TLS_RSA_WITH_NULL_SHA"
    | toString TLS_RSA_WITH_NULL_SHA256            = "TLS_RSA_WITH_NULL_SHA256"
    | toString TLS_RSA_WITH_RC4_128_MD5            = "TLS_RSA_WITH_RC4_128_MD5"
    | toString TLS_RSA_WITH_RC4_128_SHA            = "TLS_RSA_WITH_RC4_128_SHA"
    | toString TLS_RSA_WITH_3DES_EDE_CBC_SHA       = "TLS_RSA_WITH_3DES_EDE_CBC_SHA"
    | toString TLS_RSA_WITH_AES_128_CBC_SHA        = "TLS_RSA_WITH_AES_128_CBC_SHA"
    | toString TLS_RSA_WITH_AES_256_CBC_SHA        = "TLS_RSA_WITH_AES_256_CBC_SHA"
    | toString TLS_RSA_WITH_AES_128_CBC_SHA256     = "TLS_RSA_WITH_AES_128_CBC_SHA256"
    | toString TLS_RSA_WITH_AES_256_CBC_SHA256     = "TLS_RSA_WITH_AES_256_CBC_SHA256"

    | toString TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA    = "TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA"
    | toString TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA    = "TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA"
    | toString TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA   = "TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA"
    | toString TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA   = "TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA"
    | toString TLS_DH_DSS_WITH_AES_128_CBC_SHA     = "TLS_DH_DSS_WITH_AES_128_CBC_SHA"
    | toString TLS_DH_RSA_WITH_AES_128_CBC_SHA     = "TLS_DH_RSA_WITH_AES_128_CBC_SHA"
    | toString TLS_DHE_DSS_WITH_AES_128_CBC_SHA    = "TLS_DHE_DSS_WITH_AES_128_CBC_SHA"
    | toString TLS_DHE_RSA_WITH_AES_128_CBC_SHA    = "TLS_DHE_RSA_WITH_AES_128_CBC_SHA"
    | toString TLS_DH_DSS_WITH_AES_256_CBC_SHA     = "TLS_DH_DSS_WITH_AES_256_CBC_SHA"
    | toString TLS_DH_RSA_WITH_AES_256_CBC_SHA     = "TLS_DH_RSA_WITH_AES_256_CBC_SHA"
    | toString TLS_DHE_DSS_WITH_AES_256_CBC_SHA    = "TLS_DHE_DSS_WITH_AES_256_CBC_SHA"
    | toString TLS_DHE_RSA_WITH_AES_256_CBC_SHA    = "TLS_DHE_RSA_WITH_AES_256_CBC_SHA"
    | toString TLS_DH_DSS_WITH_AES_128_CBC_SHA256  = "TLS_DH_DSS_WITH_AES_128_CBC_SHA256"
    | toString TLS_DH_RSA_WITH_AES_128_CBC_SHA256  = "TLS_DH_RSA_WITH_AES_128_CBC_SHA256"
    | toString TLS_DHE_DSS_WITH_AES_128_CBC_SHA256 = "TLS_DHE_DSS_WITH_AES_128_CBC_SHA256"
    | toString TLS_DHE_RSA_WITH_AES_128_CBC_SHA256 = "TLS_DHE_RSA_WITH_AES_128_CBC_SHA256"
    | toString TLS_DH_DSS_WITH_AES_256_CBC_SHA256  = "TLS_DH_DSS_WITH_AES_256_CBC_SHA256"
    | toString TLS_DH_RSA_WITH_AES_256_CBC_SHA256  = "TLS_DH_RSA_WITH_AES_256_CBC_SHA256"
    | toString TLS_DHE_DSS_WITH_AES_256_CBC_SHA256 = "TLS_DHE_DSS_WITH_AES_256_CBC_SHA256"
    | toString TLS_DHE_RSA_WITH_AES_256_CBC_SHA256 = "TLS_DHE_RSA_WITH_AES_256_CBC_SHA256"

    | toString TLS_DH_anon_WITH_RC4_128_MD5        = "TLS_DH_anon_WITH_RC4_128_MD5"
    | toString TLS_DH_anon_WITH_3DES_EDE_CBC_SHA   = "TLS_DH_anon_WITH_3DES_EDE_CBC_SHA"
    | toString TLS_DH_anon_WITH_AES_128_CBC_SHA    = "TLS_DH_anon_WITH_AES_128_CBC_SHA"
    | toString TLS_DH_anon_WITH_AES_256_CBC_SHA    = "TLS_DH_anon_WITH_AES_256_CBC_SHA"
    | toString TLS_DH_anon_WITH_AES_128_CBC_SHA256 = "TLS_DH_anon_WITH_AES_128_CBC_SHA256"
    | toString TLS_DH_anon_WITH_AES_256_CBC_SHA256 = "TLS_DH_anon_WITH_AES_256_CBC_SHA256"

  val word8 = Word8.fromLarge o Word.toLarge

  fun length (_ : cipher_suite_identifier) = 2

  fun sub (cipher_suite_identifier, i) =
        IntAsBytes16.sub (toInt cipher_suite_identifier, i)
end

structure ClientHello :> sig
  eqtype client_hello

  val clientHello : {
    majorVersion : Word.word,
    minorVersion : Word.word,
    gmtUnixTime : Time.time,
    randomBytes : Word8Vector.vector,
    cipherSuites : CipherSuite.cipher_suite_identifier } -> client_hello

  val length : client_hello -> int
  val sub : client_hello * int -> Word8.word
  val toBytes : client_hello -> Word8Vector.vector
end = struct
  type client_hello = {
    majorVersion : Word.word,
    minorVersion : Word.word,
    gmtUnixTime : Time.time,
    randomBytes : Word8Vector.vector,
    cipherSuites : CipherSuite.cipher_suite_identifier }

  fun clientHello x = x

  val word8 = Word8.fromLarge o Word.toLarge

  fun sub ({majorVersion, minorVersion, gmtUnixTime, randomBytes, cipherSuites}, i) =
        let
          val epoch = Time.toSeconds gmtUnixTime
          val cipherSuitesLength = CipherSuite.length cipherSuites
        in
          case i of
               0 => word8 majorVersion
             | 1 => word8 minorVersion
             | 2 => LargeIntAsBytes32.sub (epoch, 0)
             | 3 => LargeIntAsBytes32.sub (epoch, 1)
             | 4 => LargeIntAsBytes32.sub (epoch, 2)
             | 5 => LargeIntAsBytes32.sub (epoch, 3)
             | 34 => word8 0w0 (* session id length *)
             | 35 => IntAsBytes16.sub (cipherSuitesLength, 0)
             | 36 => IntAsBytes16.sub (cipherSuitesLength, 1)
             | 37 => CipherSuite.sub (cipherSuites, 0)
             | 38 => CipherSuite.sub (cipherSuites, 1)
             | 39 => word8 0w1 (* compression methods length *)
             | 40 => word8 0w0 (* compression methods length *)
             | i => Word8Vector.sub (randomBytes, i - 6)
        end

  fun length clientHello = 41

  fun toBytes clientHello =
        Word8Vector.tabulate (length clientHello, fn i => (sub (clientHello, i)))
end

structure ServerHello (*:> sig
  type server_hello

  val serverHello : {
    majorVersion : Word.word,
    minorVersion : Word.word,
    randomBytes : Word8VectorSlice.slice,
    sessionId : Word8VectorSlice.slice,
    cipherSuite : CipherSuite.cipher_suite_identifier } -> server_hello

  val fromBytes : Word8VectorSlice.slice -> server_hello
end *)= struct
  type server_hello = {
    majorVersion : Word.word,
    minorVersion : Word.word,
    randomBytes : Word8VectorSlice.slice,
    sessionId : Word8VectorSlice.slice,
    cipherSuite : CipherSuite.cipher_suite_identifier }

  fun serverHello x = x
  fun randomBytes ({randomBytes, ...} : server_hello) = randomBytes

  fun fromBytes bytes =
        let
          val word = Word.fromLarge o Word8.toLarge
          val majorVersion = word (Word8VectorSlice.sub (bytes, 0))
          val minorVersion = word (Word8VectorSlice.sub (bytes, 1))
          val randomBytes = Word8VectorSlice.subslice (bytes, 2, SOME 32)
          val sessionIdLength = Word.toInt (word (Word8VectorSlice.sub (bytes, 34)))
          val sessionId = Word8VectorSlice.subslice (bytes, 35, SOME sessionIdLength)
          val cipherSuite = IntAsBytes32.tabulate (2, fn i => Word8VectorSlice.sub (bytes, 35 + sessionIdLength + i))
        in
          { majorVersion = majorVersion,
            minorVersion = minorVersion,
            randomBytes = randomBytes,
            sessionId = sessionId,
            cipherSuite = CipherSuite.fromInt cipherSuite }
        end

  fun toString {majorVersion, minorVersion, randomBytes, sessionId, cipherSuite} =
        "{ majorVersion = " ^ Word.toString majorVersion ^
        ", minorVersion = " ^ Word.toString minorVersion ^
        ", randomBytes = " ^ ExtWord8Vector.bytesToHex (Word8VectorSlice.vector randomBytes) ^
        ", sessionId = " ^ ExtWord8Vector.bytesToHex (Word8VectorSlice.vector sessionId) ^
        ", cipherSuite = " ^ CipherSuite.toString cipherSuite ^
        " }"

end

(* Slightly higher API for ASN.1 *)
structure ExtAsn1 = struct
  datatype class = Universal | Application | ContextSpecific | Private
  type tag = int
  datatype t = Bool of bool
             | Int of IntInf.int
             | BitString of Word8VectorSlice.slice
             | OctetString of Word8VectorSlice.slice
             | Null
             | Oid of Word8VectorSlice.slice
             | PrintableString of string
             | UtcTime of Word8VectorSlice.slice (* TODO *)

             | Seq of t list
             | Set of t list

             | Primitive of class * tag * Word8VectorSlice.slice
             | Constructed of class * tag * t list

  fun classToString Universal = "Universal"
    | classToString Application = "Application"
    | classToString ContextSpecific = "ContextSpecific"
    | classToString Private  = "Private"

  fun toString (Bool bool) = "Bool(" ^ Bool.toString bool ^ ")"
    | toString (Int int) = "Int(" ^ IntInf.toString int ^ ")"
    | toString (BitString slice) = "BitString(" ^ ExtWord8Vector.sliceToHex slice ^ ")"
    | toString (OctetString slice) = "OctetString(" ^ ExtWord8Vector.sliceToHex slice ^ ")"
    | toString Null = "Null"
    | toString (Oid slice) = "Oid(" ^ ExtWord8Vector.sliceToHex slice ^ ")"
    | toString (PrintableString ps) = "PrintableString(" ^ ps ^ ")"
    | toString (UtcTime slice) = "UtcTime(" ^ ExtWord8Vector.sliceToHex slice ^ ")"
    | toString (Seq ts) =
        "Seq[" ^ String.concatWith ", " (map toString ts) ^ "]"
    | toString (Set ts) =
        "Set[" ^ String.concatWith ", " (map toString ts) ^ "]"
    | toString (Primitive (class, tag, data)) =
        "Primitive(" ^ classToString class ^ ", " ^ Int.toString tag ^ ", " ^ ExtWord8Vector.sliceToHex data ^ ")"
    | toString (Constructed (class, tag, ts)) =
        "Constructed(" ^ classToString class ^ ", " ^ Int.toString tag ^ ", [" ^ String.concatWith ", " (map toString ts) ^ "])"

  fun fromBytes bytes =
        let
          fun sliceToInt slice =
                let
                  fun f (byte, acc) = acc * 256 + Int.toLarge (Word8.toInt byte)
                in
                  Word8VectorSlice.foldl f (IntInf.fromInt 0) slice
                end
          fun decodePrim (1, data) = Bool (Asn1.decodeBool data)
            | decodePrim (2, data) = Int (sliceToInt data)
            | decodePrim (3, data) = BitString data
            | decodePrim (4, data) = OctetString data
            | decodePrim (5, data) = Null
            | decodePrim (6, data) = Oid data
            | decodePrim (19, data) = PrintableString (Byte.bytesToString (Word8VectorSlice.vector data))
            | decodePrim (23, data) = UtcTime data
            | decodePrim (tag, data) = Primitive (Universal, tag, data)
          and decodeCons (16, data) = Seq (decodeItems data)
            | decodeCons (17, data) = Set (decodeItems data)
            | decodeCons (tag, data) = Constructed (Universal, tag, decodeItems data)
          and decodeItems bytes =
                case Asn1.decodeItem bytes of
                     NONE => []
                   | SOME {tag, data, remainder} =>
                       let
                         val item =
                           case tag of
                                Asn1.Universal (tag, Asn1.Primitive) => decodePrim (tag, data)
                              | Asn1.Universal (tag, Asn1.Constructed) => decodeCons (tag, data)
                              | Asn1.Application (tag, Asn1.Primitive) => Primitive (Application, tag, data)
                              | Asn1.Application (tag, Asn1.Constructed) => Constructed (Application, tag, decodeItems data)
                              | Asn1.Context (tag, Asn1.Primitive) => Primitive (ContextSpecific, tag, data)
                              | Asn1.Context (tag, Asn1.Constructed) => Constructed (ContextSpecific, tag, decodeItems data)
                              | Asn1.Private (tag, Asn1.Primitive) => Primitive (Private, tag, data)
                              | Asn1.Private (tag, Asn1.Constructed) => Constructed (Private, tag, decodeItems data)
                       in
                         item::decodeItems remainder
                       end
        in
          decodeItems bytes
        end
end

structure X509 = struct
  type algorithm_identifier = {
    algorithm : Word8VectorSlice.slice,
    parameters : ExtAsn1.t option (* TODO *)
  }

  type name = unit (* TODO *)
  type validity = {
    notBefore : Date.date,
    notAfter : Date.date
  }
  type subject_public_key_info = {
    algorithm : algorithm_identifier,
    subjectPublicKey : Word8VectorSlice.slice
  }
  type extension = {
    extnId : unit, (* TODO *)
    critical : bool,
    extnValue : ExtAsn1.t
  }
  type tbs_certificate = {
    version : IntInf.int,
    serialNumber : IntInf.int,
    signatureAlgorithm : algorithm_identifier,
    issuer : name,
    validity : validity,
    subject : name,
    subjectPublicKeyInfo : subject_public_key_info,
    issuerUniqueId : Word8VectorSlice.slice option,
    subjectUniqueID : Word8VectorSlice.slice option,
    extensions : extension list
  }
  type certificate = {
    tbsCertificate : tbs_certificate,
    signatureAlgorithm : algorithm_identifier,
    signatureValue : Word8VectorSlice.slice
  }

  fun fromBytes bytes : certificate =
        let
          val [asn1] = ExtAsn1.fromBytes bytes
          fun parseCertificate (ExtAsn1.Seq (item1::item2::item3::[])) : certificate =
                let
                  val tbsCertificate = parseTbsCertificate item1
                  val signatureAlgorithm = parseAlgorithmIdentifier item2
                  val signatureValue = parseBitString item3
                in
                  { tbsCertificate = tbsCertificate,
                    signatureAlgorithm = signatureAlgorithm,
                    signatureValue = signatureValue }
                end
            | parseCertificate asn1 =
                raise Fail ("parseCertificate: expected 3-elements Seq, found " ^ ExtAsn1.toString asn1)
          and parseTbsCertificate (ExtAsn1.Seq (i1::i2::i3::i4::i5::i6::i7::items)) : tbs_certificate =
                let
                  val version = parseVersion i1
                  val serialNumber = parseInteger i2
                  val signatureAlgorithm = parseAlgorithmIdentifier i3
                  val issuer = parseName i4
                  val validity = parseValidity i5
                  val subject = parseName i6
                  val subjectPublicKeyInfo = parseSubjectPublicKeyInfo i7
                in
                  { version = version,
                    serialNumber = serialNumber,
                    signatureAlgorithm = signatureAlgorithm,
                    issuer = issuer,
                    validity = validity,
                    subject = subject,
                    subjectPublicKeyInfo = subjectPublicKeyInfo,
                    issuerUniqueId = NONE, (* TODO *)
                    subjectUniqueID = NONE, (* TODO *)
                    extensions = [] (* TODO *) }
                end
            | parseTbsCertificate asn1 =
                raise Fail ("parseTbsCertificate: expected 7-or-more-elements Seq, found " ^ ExtAsn1.toString asn1)
          and parseVersion (ExtAsn1.Constructed (ExtAsn1.ContextSpecific, 0, [version])) =
                parseInteger version
            | parseVersion asn1 =
                raise Fail ("parseVersion: expected 1-element constructed contex-specific with tag 0, found " ^ ExtAsn1.toString asn1)
          and parseInteger (ExtAsn1.Int i) = i
            | parseInteger asn1 =
                raise Fail ("parseInteger: expected Int, found " ^ ExtAsn1.toString asn1)
          and parseValidity _ =
                { notBefore = Date.fromTimeUniv (Time.now ()), (* TODO *)
                  notAfter = Date.fromTimeUniv (Time.now ()) } (* TODO *)
          and parseName _ = () (* TODO *)
          and parseSubjectPublicKeyInfo (ExtAsn1.Seq (item1::item2::[])) =
                let
                  val algorithm = parseAlgorithmIdentifier item1
                  val subjectPublicKey = Word8VectorSlice.subslice (parseBitString item2, 1, NONE)
                in
                  { algorithm = algorithm,
                    subjectPublicKey = subjectPublicKey }
                end
            | parseSubjectPublicKeyInfo asn1 =
                raise Fail ("parseSubjectPublicKeyInfo: expected 2-elements Seq, found " ^ ExtAsn1.toString asn1)
          and parseBitString (ExtAsn1.BitString bs) = bs
            | parseBitString asn1 =
                raise Fail ("parseBitString: expected BitString, found " ^ ExtAsn1.toString asn1)
          and parseAlgorithmIdentifier (ExtAsn1.Seq (item1::item2::[])) =
                let
                  val algorithm = parseOid item1
                in
                  { algorithm = algorithm,
                    parameters = NONE } (* TODO *)
                end
            | parseAlgorithmIdentifier asn1 =
                raise Fail ("parseAlgorithmIdentifier: expected 2-elements Seq, found " ^ ExtAsn1.toString asn1)
          and parseOid (ExtAsn1.Oid oid) = oid
            | parseOid asn1 =
                raise Fail ("parseOid: expected Oid, found " ^ ExtAsn1.toString asn1)
        in
          parseCertificate asn1
        end

  fun toString _ = "-"
end

structure Certificates = struct
  type certificates = X509.certificate list

  fun fromBytes bytes =
        let
          val len = IntAsBytes32.tabulate (3, fn i => Word8VectorSlice.sub (bytes, i))
          val bytes = Word8VectorSlice.subslice (bytes, 3, NONE)
          fun loop bytes =
                if Word8VectorSlice.length bytes = 0 then []
                else
                  let
                    val len = IntAsBytes32.tabulate (3, fn i => Word8VectorSlice.sub (bytes, i))
                    val certificates = X509.fromBytes (Word8VectorSlice.subslice (bytes, 3, SOME len))
                    val rest = Word8VectorSlice.subslice (bytes, 3 + len, NONE)
                  in
                    certificates :: loop rest
                  end
        in
          loop bytes
        end

  fun toString certificates =
        "[" ^ String.concatWith ", " (map X509.toString certificates) ^ "]"
end

structure ClientKeyExchange = struct
  type public_key_encrypted_premaster_secret = Word8Vector.vector
  datatype client_key_exchange = RSAKeyExchange of public_key_encrypted_premaster_secret

  fun sub (RSAKeyExchange publicKeyEncryptedPremasterSecret, 0) =
        IntAsBytes16.sub (Word8Vector.length publicKeyEncryptedPremasterSecret, 0)
    | sub (RSAKeyExchange publicKeyEncryptedPremasterSecret, 1) =
        IntAsBytes16.sub (Word8Vector.length publicKeyEncryptedPremasterSecret, 1)
    | sub (RSAKeyExchange publicKeyEncryptedPremasterSecret, i) =
        Word8Vector.sub (publicKeyEncryptedPremasterSecret, i - 2)

  fun length (RSAKeyExchange publicKeyEncryptedPremasterSecret) =
        Word8Vector.length publicKeyEncryptedPremasterSecret + 2
end

structure Finished = struct
  type finished = Word8Vector.vector

  fun finished (masterSecret, label, handshakeMessages) : finished =
        let
          val handshakeMessages = Word8VectorSlice.concat handshakeMessages
          val label = Byte.stringToBytes label
          val prf = PRF.prf HMAC.hmacSha256
          val verifyData =
            prf (masterSecret, label, HMAC.sha256 handshakeMessages, 12)
        in
          verifyData
        end

  fun clientFinished (masterSecret, handshakeMessages) : finished =
        finished (masterSecret, "client finished", handshakeMessages)

  val sub = Word8Vector.sub
  val length = Word8Vector.length
end

structure Handshake = struct
  datatype handshake = ClientHello of ClientHello.client_hello
                     | ServerHello of ServerHello.server_hello
                     | Certificates of Certificates.certificates
                     | ServerHelloDone
                     | ClientKeyExchange of ClientKeyExchange.client_key_exchange
                     | Finished of Finished.finished

  val word8 = Word8.fromLarge o Word.toLarge

  fun sub (ClientHello clientHello, 0) = word8 0w1 (* message type of client hello *)
    | sub (ClientHello clientHello, 1) = IntAsBytes32.sub (ClientHello.length clientHello, 1)
    | sub (ClientHello clientHello, 2) = IntAsBytes32.sub (ClientHello.length clientHello, 2)
    | sub (ClientHello clientHello, 3) = IntAsBytes32.sub (ClientHello.length clientHello, 3)
    | sub (ClientHello clientHello, i) = ClientHello.sub (clientHello, i - 4)
    | sub (ClientKeyExchange clientKeyExchange, 0) = word8 0w16 (* message type of client key exchange *)
    | sub (ClientKeyExchange clientKeyExchange, 1) = IntAsBytes32.sub (ClientKeyExchange.length clientKeyExchange, 1)
    | sub (ClientKeyExchange clientKeyExchange, 2) = IntAsBytes32.sub (ClientKeyExchange.length clientKeyExchange, 2)
    | sub (ClientKeyExchange clientKeyExchange, 3) = IntAsBytes32.sub (ClientKeyExchange.length clientKeyExchange, 3)
    | sub (ClientKeyExchange clientKeyExchange, i) = ClientKeyExchange.sub (clientKeyExchange, i - 4)
    | sub (Finished finished, 0) = word8 0w20 (* message type of finished *)
    | sub (Finished finished, 1) = IntAsBytes32.sub (Finished.length finished, 1)
    | sub (Finished finished, 2) = IntAsBytes32.sub (Finished.length finished, 2)
    | sub (Finished finished, 3) = IntAsBytes32.sub (Finished.length finished, 3)
    | sub (Finished finished, i) = Finished.sub (finished, i - 4)
    | sub _ = raise Fail "unimplemented"

  fun length (ClientHello clientHello) = 4 + ClientHello.length clientHello
    | length (ClientKeyExchange clientKeyExchange) = 4 + ClientKeyExchange.length clientKeyExchange
    | length (Finished finished) = 4 + Finished.length finished
    | length _ = raise Fail "unimplemented"

  fun toBytes handshake =
        Word8Vector.tabulate (length handshake, fn i => (sub (handshake, i)))

  fun fromBytes bytes =
        let
          val word = Word.fromLarge o Word8.toLarge
          val handshakeMessageType = word (Word8VectorSlice.sub (bytes, 0))
          val len = IntAsBytes32.tabulate (3, fn i => Word8VectorSlice.sub (bytes, 1 + i))
          val rest = Word8VectorSlice.subslice (bytes, 4, NONE)
        in
          case handshakeMessageType of
               0wx02 => ServerHello (ServerHello.fromBytes rest)
             | 0wx0b => Certificates (Certificates.fromBytes rest)
             | 0wx0e => ServerHelloDone
             | _ => raise Fail "unknown handshake message type"
        end

  fun toString (ServerHello serverHello) =
        "ServerHello " ^ ServerHello.toString serverHello
    | toString (Certificates certificates) =
        "Certificates " ^ Certificates.toString certificates
    | toString ServerHelloDone = "ServerHelloDone"
    | toString _ = raise Fail "unimplemented"
end

structure Alert = struct
  datatype alert_level = Warning | Fatal

  datatype alert_desc = CloseNotify
                      | UnexpectedMessage
                      | BadRecordMac
                      | DecryptionFailed
                      | RecordOverflow
                      | DecompressionFailure
                      | HandshakeFailure
                      | NoCertificate
                      | BadCertificate
                      | UnsupportedCertificate
                      | CertificateRevoked
                      | CertificateExpired
                      | CertificateUnknown
                      | IllegalParameter
                      | UnknownCa
                      | AccessDenied
                      | DecodeError
                      | DecryptError
                      | ExportRestriction
                      | ProtocolVersion
                      | InsufficientSecurity
                      | InternalError
                      | UserCanceled
                      | NoRenegotiation
                      | UnsupportedExtension

  type alert = alert_level * alert_desc

  fun fromWord 0wx0100 = (Warning, CloseNotify)
    | fromWord 0wx010a = (Warning, UnexpectedMessage)
    | fromWord 0wx0114 = (Warning, BadRecordMac)
    | fromWord 0wx0115 = (Warning, DecryptionFailed)
    | fromWord 0wx0116 = (Warning, RecordOverflow)
    | fromWord 0wx011e = (Warning, DecompressionFailure)
    | fromWord 0wx0128 = (Warning, HandshakeFailure)
    | fromWord 0wx0129 = (Warning, NoCertificate)
    | fromWord 0wx012a = (Warning, BadCertificate)
    | fromWord 0wx012b = (Warning, UnsupportedCertificate)
    | fromWord 0wx012c = (Warning, CertificateRevoked)
    | fromWord 0wx012d = (Warning, CertificateExpired)
    | fromWord 0wx012e = (Warning, CertificateUnknown)
    | fromWord 0wx012f = (Warning, IllegalParameter)
    | fromWord 0wx0130 = (Warning, UnknownCa)
    | fromWord 0wx0131 = (Warning, AccessDenied)
    | fromWord 0wx0132 = (Warning, DecodeError)
    | fromWord 0wx0133 = (Warning, DecryptError)
    | fromWord 0wx013c = (Warning, ExportRestriction)
    | fromWord 0wx0146 = (Warning, ProtocolVersion)
    | fromWord 0wx0147 = (Warning, InsufficientSecurity)
    | fromWord 0wx0150 = (Warning, InternalError)
    | fromWord 0wx015a = (Warning, UserCanceled)
    | fromWord 0wx0164 = (Warning, NoRenegotiation)
    | fromWord 0wx016e = (Warning, UnsupportedExtension)
    | fromWord 0wx0200 = (Fatal, CloseNotify)
    | fromWord 0wx020a = (Fatal, UnexpectedMessage)
    | fromWord 0wx0214 = (Fatal, BadRecordMac)
    | fromWord 0wx0215 = (Fatal, DecryptionFailed)
    | fromWord 0wx0216 = (Fatal, RecordOverflow)
    | fromWord 0wx021e = (Fatal, DecompressionFailure)
    | fromWord 0wx0228 = (Fatal, HandshakeFailure)
    | fromWord 0wx0229 = (Fatal, NoCertificate)
    | fromWord 0wx022a = (Fatal, BadCertificate)
    | fromWord 0wx022b = (Fatal, UnsupportedCertificate)
    | fromWord 0wx022c = (Fatal, CertificateRevoked)
    | fromWord 0wx022d = (Fatal, CertificateExpired)
    | fromWord 0wx022e = (Fatal, CertificateUnknown)
    | fromWord 0wx022f = (Fatal, IllegalParameter)
    | fromWord 0wx0230 = (Fatal, UnknownCa)
    | fromWord 0wx0231 = (Fatal, AccessDenied)
    | fromWord 0wx0232 = (Fatal, DecodeError)
    | fromWord 0wx0233 = (Fatal, DecryptError)
    | fromWord 0wx023c = (Fatal, ExportRestriction)
    | fromWord 0wx0246 = (Fatal, ProtocolVersion)
    | fromWord 0wx0247 = (Fatal, InsufficientSecurity)
    | fromWord 0wx0250 = (Fatal, InternalError)
    | fromWord 0wx025a = (Fatal, UserCanceled)
    | fromWord 0wx0264 = (Fatal, NoRenegotiation)
    | fromWord 0wx026e = (Fatal, UnsupportedExtension)
    | fromWord _ = raise Fail "unknown alert"

  fun fromBytes bytes =
        let
          val word = Word.fromLarge o Word8.toLarge
          val alertLevel = word (Word8VectorSlice.sub (bytes, 0))
          val alertDesc = word (Word8VectorSlice.sub (bytes, 1))
        in
          fromWord (Word.orb (Word.<< (alertLevel, 0w8), alertDesc))
        end

  fun toString (Warning, CloseNotify)            = "(Warning, CloseNotify)"
    | toString (Warning, UnexpectedMessage)      = "(Warning, UnexpectedMessage)"
    | toString (Warning, BadRecordMac)           = "(Warning, BadRecordMac)"
    | toString (Warning, DecryptionFailed)       = "(Warning, DecryptionFailed)"
    | toString (Warning, RecordOverflow)         = "(Warning, RecordOverflow)"
    | toString (Warning, DecompressionFailure)   = "(Warning, DecompressionFailure)"
    | toString (Warning, HandshakeFailure)       = "(Warning, HandshakeFailure)"
    | toString (Warning, NoCertificate)          = "(Warning, NoCertificate)"
    | toString (Warning, BadCertificate)         = "(Warning, BadCertificate)"
    | toString (Warning, UnsupportedCertificate) = "(Warning, UnsupportedCertificate)"
    | toString (Warning, CertificateRevoked)     = "(Warning, CertificateRevoked)"
    | toString (Warning, CertificateExpired)     = "(Warning, CertificateExpired)"
    | toString (Warning, CertificateUnknown)     = "(Warning, CertificateUnknown)"
    | toString (Warning, IllegalParameter)       = "(Warning, IllegalParameter)"
    | toString (Warning, UnknownCa)              = "(Warning, UnknownCa)"
    | toString (Warning, AccessDenied)           = "(Warning, AccessDenied)"
    | toString (Warning, DecodeError)            = "(Warning, DecodeError)"
    | toString (Warning, DecryptError)           = "(Warning, DecryptError)"
    | toString (Warning, ExportRestriction)      = "(Warning, ExportRestriction)"
    | toString (Warning, ProtocolVersion)        = "(Warning, ProtocolVersion)"
    | toString (Warning, InsufficientSecurity)   = "(Warning, InsufficientSecurity)"
    | toString (Warning, InternalError)          = "(Warning, InternalError)"
    | toString (Warning, UserCanceled)           = "(Warning, UserCanceled)"
    | toString (Warning, NoRenegotiation)        = "(Warning, NoRenegotiation)"
    | toString (Warning, UnsupportedExtension)   = "(Warning, UnsupportedExtension)"
    | toString (Fatal, CloseNotify)              = "(Fatal, CloseNotify)"
    | toString (Fatal, UnexpectedMessage)        = "(Fatal, UnexpectedMessage)"
    | toString (Fatal, BadRecordMac)             = "(Fatal, BadRecordMac)"
    | toString (Fatal, DecryptionFailed)         = "(Fatal, DecryptionFailed)"
    | toString (Fatal, RecordOverflow)           = "(Fatal, RecordOverflow)"
    | toString (Fatal, DecompressionFailure)     = "(Fatal, DecompressionFailure)"
    | toString (Fatal, HandshakeFailure)         = "(Fatal, HandshakeFailure)"
    | toString (Fatal, NoCertificate)            = "(Fatal, NoCertificate)"
    | toString (Fatal, BadCertificate)           = "(Fatal, BadCertificate)"
    | toString (Fatal, UnsupportedCertificate)   = "(Fatal, UnsupportedCertificate)"
    | toString (Fatal, CertificateRevoked)       = "(Fatal, CertificateRevoked)"
    | toString (Fatal, CertificateExpired)       = "(Fatal, CertificateExpired)"
    | toString (Fatal, CertificateUnknown)       = "(Fatal, CertificateUnknown)"
    | toString (Fatal, IllegalParameter)         = "(Fatal, IllegalParameter)"
    | toString (Fatal, UnknownCa)                = "(Fatal, UnknownCa)"
    | toString (Fatal, AccessDenied)             = "(Fatal, AccessDenied)"
    | toString (Fatal, DecodeError)              = "(Fatal, DecodeError)"
    | toString (Fatal, DecryptError)             = "(Fatal, DecryptError)"
    | toString (Fatal, ExportRestriction)        = "(Fatal, ExportRestriction)"
    | toString (Fatal, ProtocolVersion)          = "(Fatal, ProtocolVersion)"
    | toString (Fatal, InsufficientSecurity)     = "(Fatal, InsufficientSecurity)"
    | toString (Fatal, InternalError)            = "(Fatal, InternalError)"
    | toString (Fatal, UserCanceled)             = "(Fatal, UserCanceled)"
    | toString (Fatal, NoRenegotiation)          = "(Fatal, NoRenegotiation)"
    | toString (Fatal, UnsupportedExtension)     = "(Fatal, UnsupportedExtension)"
end

structure TLSMessage = struct
  datatype tls_message = ChangeCipherSpec
                       | Alert of Alert.alert
                       | Handshake of Handshake.handshake

  val word8 = Word8.fromLarge o Word.toLarge

  val contentChangeCipherSpec = word8 0w20
  val contentAlert = word8 0w21
  val contentHandshake = word8 0w22
  val contentApplicationData = word8 0w23

  fun sub (ChangeCipherSpec, 0) = contentChangeCipherSpec
    | sub (ChangeCipherSpec, 1) = word8 0w3
    | sub (ChangeCipherSpec, 2) = word8 0w3
    | sub (ChangeCipherSpec, 3) = word8 0w0
    | sub (ChangeCipherSpec, 4) = word8 0w1
    | sub (ChangeCipherSpec, 5) = word8 0w1
    | sub (Handshake handshake, 0) = contentHandshake
    | sub (Handshake handshake, 1) = word8 0w3
    | sub (Handshake handshake, 2) = word8 0w3
    | sub (Handshake handshake, 3) = IntAsBytes16.sub (Handshake.length handshake, 0)
    | sub (Handshake handshake, 4) = IntAsBytes16.sub (Handshake.length handshake, 1)
    | sub (Handshake handshake, i) = Handshake.sub (handshake, i - 5)
    | sub _ = raise Fail "unimplemented"

  fun length ChangeCipherSpec = 6
    | length (Handshake handshake) = 5 + Handshake.length handshake
    | length _ = raise Fail "unimplemented"

  fun toBytes tlsMessage =
        Word8Vector.tabulate (length tlsMessage, fn i => (sub (tlsMessage, i)))

  fun fromStream ins =
        let
          val word = Word.fromLarge o Word8.toLarge
          val (tlsHeader, ins) = BinStream.inputN (ins, 5)
          val contentType = word (Word8Vector.sub (tlsHeader, 0))
          val majorVersion = word (Word8Vector.sub (tlsHeader, 1))
          val minorVersion = word (Word8Vector.sub (tlsHeader, 2))
          val len = IntAsBytes16.tabulate (2, fn i => Word8Vector.sub (tlsHeader, 3 + i))
          val (rest, ins) = BinStream.inputN (ins, len)
          val rest = Word8VectorSlice.full rest
          val tlsMessage =
            case contentType of
                 0w20 => ChangeCipherSpec
               | 0w21 => Alert (Alert.fromBytes rest)
               | 0w22 => Handshake (Handshake.fromBytes rest)
               | 0w23 => raise Fail "application-data unimplemented"
               | _ => raise Fail "unknown content type"
        in
          (tlsMessage, rest, ins)
        end

  fun toString ChangeCipherSpec = "ChangeCipherSpec"
    | toString (Alert alert) =
        "Alert (" ^ Alert.toString alert ^ ")"
    | toString (Handshake handshake) =
        "Handshake (" ^ Handshake.toString handshake ^ ")"
end

structure Main = struct
  val word8 = Word8.fromLarge o Word.toLarge
  fun main (host, port) =
        let
          fun connect (host, port) =
                let
                  val sock = INetSock.TCP.socket ()
                  val addr =
                    case NetHostDB.getByName host of
                         NONE => raise OS.SysErr (host ^ " not found", NONE)
                       | SOME entry =>
                           INetSock.toAddr (NetHostDB.addr entry, port)
                in
                  Socket.connect (sock, addr);
                  sock
                end
          val sock = connect (host, port)
          val ins = BinStream.fromFun (fn () => Socket.recvVec (sock, 2048))
          val () = print ("socket connected\n")
          (* Step 1. Send the TLS handshake "client hello" message *)
          val clientHello = ClientHello.clientHello {
            majorVersion = 0w3,
            minorVersion = 0w3,
            gmtUnixTime = Time.now (),
            randomBytes = Byte.stringToBytes "0123456789012345678901234567",
            cipherSuites = CipherSuite.TLS_RSA_WITH_AES_128_CBC_SHA }
            (*cipherSuites = CipherSuite.TLS_RSA_WITH_3DES_EDE_CBC_SHA }*)
            (*cipherSuites = CipherSuite.TLS_RSA_WITH_DES_CBC_SHA }*)
          fun sendClientHello (sock, clientHello) =
                let
                  val handshake = Handshake.ClientHello clientHello
                  val tlsMessage = TLSMessage.Handshake handshake
                  val bytes = TLSMessage.toBytes tlsMessage
                  val len = Socket.sendVec (sock, Word8VectorSlice.full bytes)
                in
                  if len = Word8Vector.length bytes then
                    Word8VectorSlice.slice (bytes, 5, NONE) (* strip record header *)
                  else
                    raise Fail "failed to send client hello"
                end
          val sentClientHello = sendClientHello (sock, clientHello)
          val clientRandom = Word8Vector.tabulate (32, fn i => ClientHello.sub (clientHello, i + 2))
          (* Step 2. Receive the server hello response *)
          fun receiveServerHello ins =
                let
                  val (TLSMessage.Handshake (Handshake.ServerHello serverHello), receivedServerHello, ins) = TLSMessage.fromStream ins
                  val () = print (ServerHello.toString serverHello ^ "\n")
                in
                  receiveServerCertificates (ins, serverHello, receivedServerHello)
                end
          and receiveServerCertificates (ins, serverHello, receivedServerHello) =
                let
                  val (serverCertificates, receivedServerCertificates, ins) = TLSMessage.fromStream ins
                  val () = print (TLSMessage.toString serverCertificates ^ "\n")
                in
                  receiveServerHelloDone (ins, serverHello, receivedServerHello, serverCertificates, receivedServerCertificates)
                end
          and receiveServerHelloDone (ins, serverHello, receivedServerHello, serverCertificates, receivedServerCertificates) =
                let
                  val (serverHelloDone, receivedServerHelloDone, ins) = TLSMessage.fromStream ins
                  val () = print (TLSMessage.toString serverHelloDone ^ "\n")
                in
                  (ins, serverHello, receivedServerHello, serverCertificates, receivedServerCertificates, serverHelloDone, receivedServerHelloDone)
                end
          val (ins, serverHello, receivedServerHello, serverCertificates, receivedServerCertificates, serverHelloDone, receivedServerHelloDone) = receiveServerHello ins
          val serverRandom = Word8VectorSlice.vector (ServerHello.randomBytes serverHello)
          (* Step 3. Send client key exchange, change cipher spec (7.1) and
             encrypted handshake message *)
          val premasterSecret =
                let
                  fun f 0 = word8 0w3 (* protocol version *)
                    | f 1 = word8 0w3 (* protocol version *)
                    | f i = word8 (Word.fromInt i) (* TODO: SHOULD BE RANDOM! *)
                in
                  Word8Vector.tabulate (48, f)
                end
          fun sendClientKeyExchange (sock, premasterSecret, serverCertificates) =
                let
                  val TLSMessage.Handshake (Handshake.Certificates certs) = serverCertificates
                  fun getPublicKey (cert : X509.certificate) =
                        let
                          val subjectPublicKey =
                            #subjectPublicKey (#subjectPublicKeyInfo (#tbsCertificate cert))
                          val key = case ExtAsn1.fromBytes subjectPublicKey of
                                         [ExtAsn1.Seq ([ExtAsn1.Int modulus, ExtAsn1.Int exponent])] =>
                                           { modulus = RSA.packIntInf (modulus, 64),
                                             exponent = RSA.packIntInf (exponent, 64) }
                                       | _ => raise Fail ""
                        in
                          key
                        end
                  val publicKey = getPublicKey (hd certs)
                  val encryptedPremasterSecret = RSA.encryptBytes (premasterSecret, publicKey)
                  val clientKeyExchange = ClientKeyExchange.RSAKeyExchange encryptedPremasterSecret
                  val handshake = Handshake.ClientKeyExchange clientKeyExchange
                  val tlsMessage = TLSMessage.Handshake handshake
                  val bytes = TLSMessage.toBytes tlsMessage
                  val len = Socket.sendVec (sock, Word8VectorSlice.full bytes)
                in
                  if len = Word8Vector.length bytes then
                    Word8VectorSlice.slice (bytes, 5, NONE)
                  else
                    raise Fail "failed to send client key exchange"
                end
          val sentClientKeyExchange = sendClientKeyExchange (sock, premasterSecret, serverCertificates)
          val prf = PRF.prf HMAC.hmacSha256
          val masterSecretLabel = Byte.stringToBytes "master secret"
          val masterSecret = prf (premasterSecret, masterSecretLabel, Word8Vector.concat [clientRandom, serverRandom], 48)
          fun sendChangeCipherSpec sock =
                let
                  val tlsMessage = TLSMessage.ChangeCipherSpec
                  val bytes = TLSMessage.toBytes tlsMessage
                  val len = Socket.sendVec (sock, Word8VectorSlice.full bytes)
                in
                  if len = Word8Vector.length bytes then
                    Word8VectorSlice.slice (bytes, 5, NONE) (* strip record header *)
                  else
                    raise Fail "failed to send client change cipher spec"
                end
          val sentChangeCipherSpec = sendChangeCipherSpec sock
          val handshakeMessages = [
             sentClientHello,
             receivedServerHello,
             receivedServerCertificates,
             receivedServerHelloDone,
             sentClientKeyExchange,
             sentChangeCipherSpec ]
          fun sendFinished (sock, masterSecret, handshakeMessages) =
                let
                  val clientFinished =
                    Finished.clientFinished (masterSecret, handshakeMessages)
                  val handshake = Handshake.Finished clientFinished
                  val tlsMessage = TLSMessage.Handshake handshake
                  val bytes = TLSMessage.toBytes tlsMessage
                  val len = Socket.sendVec (sock, Word8VectorSlice.full bytes)
                in
                  if len = Word8Vector.length bytes then
                    Word8VectorSlice.slice (bytes, 5, NONE) (* strip record header *)
                  else
                    raise Fail "failed to send client finished"
                end
          val sentFinished = sendFinished (sock, masterSecret, handshakeMessages)
        in
          ()
          (*
          print (Int.toString (Word8Vector.length received) ^ "\n");
          print (ExtWord8Vector.bytesToHex received);
          *)
          before Socket.close sock
        end
end

val _ = Main.main ("www.google.com", 443)

