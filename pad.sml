structure Pad :> sig
  (* pad (curr, prev) *)
  type pad = Word8Vector.vector * Word8Vector.vector -> Word8Vector.vector

  type unpad = Word8Vector.vector -> Word8Vector.vector

  (* NIST 800-38A *)
  val padNist : pad
  val unpadNist : unpad
end = struct
  type pad = Word8Vector.vector * Word8Vector.vector -> Word8Vector.vector
  type unpad = Word8Vector.vector -> Word8Vector.vector

  val word8 = Word8.fromLarge o Word.toLarge

  fun padNist (vec, prev) =
        let
          val vecLen = Word8Vector.length vec
          fun hasAmbiguousSuffix vec =
                let
                  fun loop 0 = false
                    | loop i =
                        let
                          val b = Word8Vector.sub (vec, i - 1)
                        in
                          if b = (word8 0wx80) then true
                          else if b = (word8 0wx00) then loop (i - 1)
                          else false
                        end
                in
                  loop (Word8Vector.length vec)
                end
        in
          if vecLen = 0 then
            if hasAmbiguousSuffix prev then
              Byte.stringToBytes "\128\000\000\000\000\000\000\000" (* 0x80 0x00.. *)
            else
              vec
          else
            let
              val padLen = 8 - vecLen
              fun f i =
                    if i < vecLen then
                      Word8Vector.sub (vec, i)
                    else if i = vecLen then word8 0wx80
                    else word8 0wx00
            in
              if padLen <= 0 then vec
              else
                Word8Vector.tabulate (8, f)
            end
        end

  fun unpadNist vec =
        let
          fun prefix i =
                Word8VectorSlice.vector (Word8VectorSlice.slice (vec, 0, SOME i))
          fun loop i =
                if Word8Vector.sub (vec, i) = word8 0wx80 then
                  prefix i
                else if Word8Vector.sub (vec, i) = word8 0wx00 then
                  loop (i - 1)
                else
                  vec
        in
          loop (Word8Vector.length vec - 1)
        end

end
