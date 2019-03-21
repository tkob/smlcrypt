structure PRF = struct
  val (op ^) = ExtWord8Vector.^
  infix ^

  fun pHash hmacHash (secret, seed, len) =
        let
          fun a 0 = seed
            | a i = hmacHash secret (a (i - 1))
          fun pHash' (i, len) =
                let
                  val h = hmacHash secret (a i ^ seed)
                  val len = len - Word8Vector.length h
                in
                  if len <= 0 then h
                  else
                    h ^ pHash' (i + 1, len)
                end
        in
          ExtWord8Vector.truncate (pHash' (1, len), len)
        end

  fun prf hmacHash (secret, label, seed, len) =
        pHash hmacHash (secret, label ^ seed, len)

  fun prfString hmacHash (secret, label, seed, len) =
        Byte.bytesToString (pHash hmacHash (Byte.stringToBytes secret, Byte.stringToBytes (String.^ (label, seed)), len))
end
