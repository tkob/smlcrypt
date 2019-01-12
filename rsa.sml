structure RSA = struct
  val word8 = Word8.fromLarge o Word.toLarge
  val word = Word.fromLarge o Word8.toLarge
  val << = Word8.<<
  val >> = Word8.>>
  val orb = Word8.orb
  val andb = Word8.andb
  val xorb = Word8.xorb
  val notb = Word8.notb
  infix << >> orb andb xorb

  type key = { modulus : Word8Vector.vector, exponent : Word8Vector.vector }
  type instream = {
    src : BinStream.instream,
    key : key,
    operate : BinStream.instream * key -> Word8Vector.vector * BinStream.instream
  }

  fun rsaCompute (m, e, n) =
        (* Compute c = m^e mod n. *)
        let
          open IntInf
        in
          if e = fromInt 0 then fromInt 1
          else if e = fromInt 1 then m mod n
          else
            let
              (* m^e = m^(e1+e1+e2) = m^e1 * m^e1 * m^e2 where e=e1+e1+e2 *)
              val e1 = e div (fromInt 2)
              val e2 = e - e1 - e1
              val c1 = rsaCompute (m, e1, n)
              val c2 = rsaCompute (m, e2, n)
            in
              (((c1 * c1) mod n) * c2) mod n
            end
        end

  (* block = header(0 byte + padding identifer) + padding(at least 11 bytes) + data *)

  fun unpackIntInf bytes =
        Word8Vector.foldl
          (fn (b, acc) => IntInf.<< (acc, 0w8) + IntInf.fromInt (Word8.toInt b))
          (IntInf.fromInt 0)
          bytes

  fun packIntInf (int, packedLen) =
        let
          val len = ((IntInf.log2 int) div 8) + 1
          val offset = packedLen - len
          fun f i =
                if i < offset then (word8 0w0)
                else
                  let
                    val toByte = Word8.fromInt o IntInf.toInt
                    val shift = Word.fromInt ((len - (i - offset) - 1) * 8)
                  in
                    toByte (IntInf.andb (IntInf.~>> (int, shift), IntInf.fromInt 0xff))
                  end
        in
          Word8Vector.tabulate (packedLen, f)
        end

  fun openString' operate (s, key : key) =
        let
          val ins = TextIO.openString s
          val src = BinStream.fromFun (fn () => Byte.stringToBytes (TextIO.input ins))
        in
          { src = src,
            key = key,
            operate = operate }
        end

  fun openBytes' operate (bytes, key : key) =
        openString' operate (Byte.bytesToString bytes, key)

  fun input {src, key, operate} =
        let
          val (output, src') = operate {src=src, key=key}
        in
          (output, {src=src', key=key, operate=operate})
        end

  fun inputAll ins =
        let
          fun inputAll' (ins, acc) =
                let
                  val (block, ins') = input ins
                in
                  if Word8Vector.length block = 0 then
                    (Word8Vector.concat (rev acc), ins')
                  else
                    inputAll' (ins', block::acc)
                end
        in
          inputAll' (ins, [])
        end

  fun rsaBlockEncrypt {src, key = {modulus, exponent} : key} =
        let
          val modulusLength = Word8Vector.length modulus
          val (input, src') = BinStream.inputN (src, modulusLength - 11)
          val blockSize = Word8Vector.length input
        in
          if blockSize = 0 then (Byte.stringToBytes "", src')
          else
            let
              fun pad 0 = word8 0w0
                | pad 1 = word8 0w2
                | pad n =
                    if n < modulusLength - blockSize - 1 then Word8.fromInt n
                    else if n = modulusLength - blockSize - 1 then word8 0w0
                    else
                      Word8Vector.sub (input, n - (modulusLength - blockSize))
              val paddedBlock = Word8Vector.tabulate (modulusLength, pad)
              val m = unpackIntInf paddedBlock
              val e = unpackIntInf exponent
              val n = unpackIntInf modulus
              val c = rsaCompute (m, e, n)
            in
              (packIntInf (c, 64), src')
            end
        end

  fun rsaBlockDecrypt {src, key = {modulus, exponent} : key} =
        let
          val modulusLength = Word8Vector.length modulus
          val (block, src') = BinStream.inputN (src, modulusLength)
          val blockSize = Word8Vector.length block
        in
          if blockSize = 0 then (Byte.stringToBytes "", src')
          else
            let
              val () = if Word8Vector.length block = modulusLength then ()
                       else raise Fail "invalid length"
              val c = unpackIntInf block
              val e = unpackIntInf exponent
              val n = unpackIntInf modulus
              val m = rsaCompute (c, e, n)
              val paddedBlock = packIntInf (m, 64)
              fun unpad bytes =
                    consumeHeader (Word8VectorSlice.full bytes)
              and consumeHeader slice =
                    if Word8VectorSlice.sub (slice, 0) = word8 0w0
                    then consumePaddingId (Word8VectorSlice.subslice (slice, 1, NONE))
                    else raise Fail "unrecognized block: the 1st byte is not 0x00"
              and consumePaddingId slice =
                    if Word8VectorSlice.sub (slice, 0) = word8 0w2
                    then consumePadding (Word8VectorSlice.subslice (slice, 1, NONE))
                    else raise Fail "unrecognized block: the 2nd byte is not 0x02"
              and consumePadding slice =
                    if Word8VectorSlice.sub (slice, 0) = word8 0w0
                    then Word8VectorSlice.vector (Word8VectorSlice.subslice (slice, 1, NONE))
                    else consumePadding (Word8VectorSlice.subslice (slice, 1, NONE))
            in
              (unpad paddedBlock, src')
            end
        end

  structure Encrypt = struct
    type instream = instream

    fun openString (s, key) =
          openString' rsaBlockEncrypt (s, key)

    fun openBytes (s, key) =
          openBytes' rsaBlockEncrypt (s, key)
  end

  structure Decrypt = struct
    type instream = instream

    fun openString (s, key) =
          openString' rsaBlockDecrypt (s, key)

    fun openBytes (s, key) =
          openBytes' rsaBlockDecrypt (s, key)
  end

  fun encryptString (s, key) =
        let
          val ins = Encrypt.openString (s, key)
          val (vec, _) = inputAll ins
        in
          Byte.bytesToString vec
        end

  fun decryptString (s, key) =
        let
          val ins = Decrypt.openString (s, key)
          val (vec , _) = inputAll ins
        in
          Byte.bytesToString vec
        end

end
