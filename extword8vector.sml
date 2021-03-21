structure ExtWord8Vector :> sig

  include MONO_VECTOR
    where type vector = Word8Vector.vector
    where type elem = Word8.word

  val word8 : Word.word -> elem
  val xor : vector * vector -> vector
  val bytesToHex : vector -> string
  val sliceToHex : Word8VectorSlice.slice -> string
  val hexToBytes : string -> vector
  val rotateLeft : vector -> vector
  val rotateRight : vector -> vector
  val truncate : vector * int -> vector

  val xorBytes : vector * vector -> vector
  val ^ : vector * vector -> vector
  val rightPadBytes : elem -> int -> vector -> vector
  val bytes : int * elem -> vector
  val base16 : vector -> string
  val base16lower : vector -> string

  val sliceEqVec : Word8VectorSlice.slice * vector -> bool
end = struct

  open Word8Vector

  val word8 = Word8.fromLarge o Word.toLarge
  val word = Word.fromLarge o Word8.toLarge
  val << = Word8.<<
  val >> = Word8.>>
  val orb = Word8.orb
  val andb = Word8.andb
  val notb = Word8.notb
  infix << >> orb andb

  fun xor (vec1, vec2) =
        let
          val len1 = Word8Vector.length vec1
          val len2 = Word8Vector.length vec2
          fun xorByte i =
                let
                  val b1 = Word8Vector.sub (vec1, i)
                  val b2 = Word8Vector.sub (vec2, i)
                in
                  Word8.xorb (b1, b2)
                end
        in
          if len1 <> len2 then raise Fail "unequal lengths"
          else
            Word8Vector.tabulate (len1, xorByte)
        end

(*
  fun byteToHex b =
        if b < word8 0wx10
        then "0" ^ Word8.toString b
        else Word8.toString b

  fun bytesToHex bytes = concat (Word8Vector.foldr
                                  (fn (b, acc) => byteToHex b::acc)
                                  []
                                  bytes)
*)

  fun hexToBytes h =
        let
          fun hexToExplodedString s =
                if Substring.isEmpty s then []
                else
                  let
                    val (hexByte, rest) = Substring.splitAt (s, 2)
                    val hexByte = Substring.string hexByte
                    val SOME w = Word8.fromString hexByte
                  in
                    Char.chr (Word8.toInt w)::hexToExplodedString rest
                  end
        in
          Byte.stringToBytes (implode (hexToExplodedString (Substring.full h)))
        end

  fun rotateLeft bytes =
        let
          val len = Word8Vector.length bytes
          fun f i = Word8Vector.sub (bytes, (i + 1) mod len)
        in
          Word8Vector.tabulate (len, f)
        end

  fun rotateRight bytes =
        let
          val len = Word8Vector.length bytes
          fun f i = Word8Vector.sub (bytes, (i - 1 + len) mod len)
        in
          Word8Vector.tabulate (len, f)
        end

  fun truncate (bytes, len) =
        Word8Vector.tabulate (len, fn i => Word8Vector.sub (bytes, i))

  fun toList bytes = Word8Vector.foldr (op ::) [] bytes

  fun xorBytes (a, b) =
        let
          val len = Word8Vector.length a
          fun subXor i =
                Word8.xorb (Word8Vector.sub (a, i), Word8Vector.sub (b, i))
        in
          if len <> Word8Vector.length b then raise Fail ""
          else
            Word8Vector.tabulate (len, subXor)
        end

  infix ^
  fun (a ^ b) = Word8Vector.concat [a, b]

  fun rightPadBytes byte targetLen bytes =
        let
          val sourceLen = Word8Vector.length bytes
          fun take i =
                if i < sourceLen
                then Word8Vector.sub (bytes, i)
                else byte
        in
          Word8Vector.tabulate (targetLen, take)
        end

  fun bytes (len, init) = Word8Vector.tabulate (len, fn _ => init)

  fun base16 bytes = String.concat (List.map (StringCvt.padLeft #"0" 2 o Word8.toString) (toList bytes))
  fun base16lower bytes = String.map Char.toLower (base16 bytes)

  val bytesToHex = base16
  fun sliceToHex slice = bytesToHex (Word8VectorSlice.vector slice)

  fun sliceEqVec (slice, vec) =
        Word8VectorSlice.length slice = Word8Vector.length vec
        andalso
        Word8VectorSlice.foldli
          (fn (i, e, a) => a andalso e = Word8Vector.sub (vec, i))
          true
          slice

end
