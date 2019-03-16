structure ExtWord8Vector = struct
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

  fun byteToHex b =
        if b < word8 0wx10
        then "0" ^ Word8.toString b
        else Word8.toString b

  fun bytesToHex bytes = concat (Word8Vector.foldr
                                  (fn (b, acc) => byteToHex b::acc)
                                  []
                                  bytes)

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

end
