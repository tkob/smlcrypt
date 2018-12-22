functor DESFn(S : sig
  val blockEncrypt :
        ((Word8Vector.vector * Word8Vector.vector -> Word8Vector.vector) *
         (Word8Vector.vector * Word8Vector.vector -> Word8Vector.vector))
        -> (Word8Vector.vector * Word8Vector.vector * Word8Vector.vector)
        -> Word8Vector.vector

  val blockDecrypt :
        ((Word8Vector.vector * Word8Vector.vector -> Word8Vector.vector) *
         (Word8Vector.vector * Word8Vector.vector -> Word8Vector.vector))
        -> (Word8Vector.vector * Word8Vector.vector * Word8Vector.vector)
        -> Word8Vector.vector
end) :> sig

  structure Encrypt : sig
    type instream

    val input : instream -> Word8Vector.vector * instream
    val inputAll : instream -> Word8Vector.vector * instream

    (* openString pad (plain, iv, key) *)
    val openString : Pad.pad -> string * string * string -> instream
    val openBytes : Pad.pad
                    -> Word8Vector.vector * Word8Vector.vector * Word8Vector.vector
                    -> instream
  end

  structure Decrypt : sig
    type instream

    val input : instream -> Word8Vector.vector * instream
    val inputAll : instream -> Word8Vector.vector * instream

    (* openString pad (plain, iv, key) *)
    val openString : Pad.unpad -> string * string * string -> instream
    val openBytes : Pad.unpad
                    -> Word8Vector.vector * Word8Vector.vector * Word8Vector.vector
                    -> instream
  end

  val encryptString : Pad.pad -> string * string * string -> string
  val decryptString : Pad.unpad -> string * string * string -> string

end = struct
  val word8 = Word8.fromLarge o Word.toLarge
  val word = Word.fromLarge o Word8.toLarge
  val << = Word8.<<
  val >> = Word8.>>
  val orb = Word8.orb
  val andb = Word8.andb
  val notb = Word8.notb
  infix << >> orb andb
  val xor = ExtWord8Vector.xor

  fun permute (src, table) =
        let
          val bitLen = Vector.length table
          val byteLen = Int.quot (bitLen, 8)
          val numRemBits = Int.rem (bitLen, 8)
          val numAllocBytes =
            if numRemBits > 0 then byteLen + 1 else byteLen
          fun g i =
                let
                  fun get pos =
                        (* returns 0wx80 if the bit at pos is set,
                         *         0wx00 otherwise *)
                        let
                          val byteIndex = pos div 8
                          val bitPos = Word.fromInt (pos mod 8)
                          val byte = Word8Vector.sub (src, byteIndex)
                        in
                          (byte << bitPos) andb (word8 0wx80)
                        end
                  val base = i * 8
                  fun pushBits (byte, offset) =
                        let
                          val byte = byte >> 0w1
                          val byte = byte orb (get (Vector.sub (table, base + offset) - 1))
                        in
                          if offset = 0 then
                            byte
                          else
                            pushBits (byte, offset - 1)
                        end
                  val offset = if i = byteLen then numRemBits - 1 else 7
                  val byte = pushBits (word8 0w0, offset)
                in
                  byte
                end
        in
          Word8Vector.tabulate (numAllocBytes, g)
        end

  (* Initial permutation - 64 bits to 64 bits *)
  val ipTable = Vector.fromList [
    58, 50, 42, 34, 26, 18, 10, 2,
    60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6,
    64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17,  9, 1,
    59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5,
    63, 55, 47, 39, 31, 23, 15, 7]

  val fpTable = Vector.fromList [
    40, 8, 48, 16, 56, 24, 64, 32,
    39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30,
    37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28,
    35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26,
    33, 1, 41,  9, 49, 17, 57, 25]

  (* 64 bits to 56 bits *)
  val pc1Table = Vector.fromList [
    57, 49, 41, 33, 25, 17 , 9,  1,
    58, 50, 42, 34, 26, 18, 10,  2,
    59, 51, 43, 35, 27, 19, 11,  3,
    60, 52, 44, 36, 63, 55, 47, 39,
    31, 23, 15,  7, 62, 54, 46, 38,
    30, 22, 14,  6, 61, 53, 45, 37,
    29, 21, 13,  5, 28, 20, 12,  4]

  (* 56 bits to 48 bits *)
  val pc2Table = Vector.fromList [
    14, 17, 11, 24,  1,  5,
     3, 28, 15,  6, 21, 10,
    23, 19, 12,  4, 26,  8,
    16,  7, 27, 20, 13,  2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32]

  (* 32 bits to 48 bits *)
  val expansionTable = Vector.fromList [
       32,  1,  2,  3,  4,  5,
        4,  5,  6,  7,  8,  9, 
        8,  9, 10, 11, 12, 13, 
       12, 13, 14, 15, 16, 17, 
       16, 17, 18, 19, 20, 21,
       20, 21, 22, 23, 24, 25,
       24, 25, 26, 27, 28, 29,
       28, 29, 30, 31, 32,  1]

  val sbox = Vector.fromList [
    Vector.fromList [
      14, 0, 4, 15, 13, 7, 1, 4, 2, 14, 15, 2, 11, 13, 8, 1,
       3, 10, 10, 6, 6, 12, 12, 11, 5, 9, 9, 5, 0, 3, 7, 8,
       4, 15, 1, 12, 14, 8, 8, 2, 13, 4, 6, 9, 2, 1, 11, 7,
      15, 5, 12, 11, 9, 3, 7, 14, 3, 10, 10, 0, 5, 6, 0, 13],
    Vector.fromList [
      15, 3, 1, 13, 8, 4, 14, 7, 6, 15, 11, 2, 3, 8, 4, 14,
       9, 12, 7, 0, 2, 1, 13, 10, 12, 6, 0, 9, 5, 11, 10, 5,
       0, 13, 14, 8, 7, 10, 11, 1, 10, 3, 4, 15, 13, 4, 1, 2,
       5, 11, 8, 6, 12, 7, 6, 12, 9, 0, 3, 5, 2, 14, 15, 9],
    Vector.fromList [
      10, 13, 0, 7, 9, 0, 14, 9, 6, 3, 3, 4, 15, 6, 5, 10,
       1, 2, 13, 8, 12, 5, 7, 14, 11, 12, 4, 11, 2, 15, 8, 1,
      13, 1, 6, 10, 4, 13, 9, 0, 8, 6, 15, 9, 3, 8, 0, 7,
      11, 4, 1, 15, 2, 14, 12, 3, 5, 11, 10, 5, 14, 2, 7, 12],
    Vector.fromList [
       7, 13, 13, 8, 14, 11, 3, 5, 0, 6, 6, 15, 9, 0, 10, 3,
       1, 4, 2, 7, 8, 2, 5, 12, 11, 1, 12, 10, 4, 14, 15, 9,
      10, 3, 6, 15, 9, 0, 0, 6, 12, 10, 11, 1, 7, 13, 13, 8,
      15, 9, 1, 4, 3, 5, 14, 11, 5, 12, 2, 7, 8, 2, 4, 14],
    Vector.fromList [
       2, 14, 12, 11, 4, 2, 1, 12, 7, 4, 10, 7, 11, 13, 6, 1,
       8, 5, 5, 0, 3, 15, 15, 10, 13, 3, 0, 9, 14, 8, 9, 6,
       4, 11, 2, 8, 1, 12, 11, 7, 10, 1, 13, 14, 7, 2, 8, 13,
      15, 6, 9, 15, 12, 0, 5, 9, 6, 10, 3, 4, 0, 5, 14, 3],
    Vector.fromList [
      12, 10, 1, 15, 10, 4, 15, 2, 9, 7, 2, 12, 6, 9, 8, 5,
       0, 6, 13, 1, 3, 13, 4, 14, 14, 0, 7, 11, 5, 3, 11, 8,
       9, 4, 14, 3, 15, 2, 5, 12, 2, 9, 8, 5, 12, 15, 3, 10,
       7, 11, 0, 14, 4, 1, 10, 7, 1, 6, 13, 0, 11, 8, 6, 13],
    Vector.fromList [
       4, 13, 11, 0, 2, 11, 14, 7, 15, 4, 0, 9, 8, 1, 13, 10,
       3, 14, 12, 3, 9, 5, 7, 12, 5, 2, 10, 15, 6, 8, 1, 6,
       1, 6, 4, 11, 11, 13, 13, 8, 12, 1, 3, 4, 7, 10, 14, 7,
      10, 9, 15, 5, 6, 0, 8, 15, 0, 14, 5, 2, 9, 3, 2, 12],
    Vector.fromList [
      13, 1, 2, 15, 8, 13, 4, 8, 6, 10, 15, 3, 11, 7, 1, 4,
      10, 12, 9, 5, 3, 6, 14, 11, 5, 0, 0, 14, 12, 9, 7, 2,
       7, 2, 11, 1, 4, 14, 1, 7, 9, 4, 12, 10, 14, 8, 2, 13,
       0, 15, 6, 12, 10, 9, 13, 0, 15, 3, 3, 5, 5, 6, 8, 11]
    ]

  (* 32 bit to 32 bit *)
  val pTable = Vector.fromList [
    16,  7, 20, 21,
    29, 12, 28, 17,
     1, 15, 23, 26,
     5, 18, 31, 10,
     2,  8, 24, 14,
    32, 27,  3,  9,
    19, 13, 30,  6,
    22, 11,  4, 25]

  fun sliceBits (bytes, offset, len) =
        let
          fun take (offset, len, w) =
                let
                  val i = Int.quot (offset, 8)
                  val j = Int.rem (offset, 8)
                  val byte = Word8Vector.sub (bytes, i)
                  val mask = word8 0wxff >> Word.fromInt j
                  val byte = byte andb mask
                  val k = 8 - j
                in
                  if len > k then
                    take (offset + k, len - k, Word.orb (Word.<< (w, Word.fromInt k), word byte))
                  else
                    Word.>> (Word.orb (Word.<< (w, Word.fromInt k), word byte), Word.fromInt (k - len))
                end
        in
          take (offset, len, 0w0)
        end

  fun rol vec =
        let
          val sub = Word8Vector.sub
          val carryLeft = (sub (vec, 0) andb (word8 0wx80)) >> 0w3
          val carryRight = (sub (vec, 3) andb (word8 0wx08)) >> 0w3
          fun f 0 =
                (sub (vec, 0) << 0w1) orb ((sub (vec, 1) andb (word8 0wx80)) >> 0w7)
            | f 1 =
                (sub (vec, 1) << 0w1) orb ((sub (vec, 2) andb (word8 0wx80)) >> 0w7)
            | f 2 =
                (sub (vec, 2) << 0w1) orb ((sub (vec, 3) andb (word8 0wx80)) >> 0w7)
            | f 3 =
                (((sub (vec, 3) << 0w1) orb ((sub (vec, 4) andb (word8 0wx80)) >> 0w7)) andb notb (word8 0wx10)) orb carryLeft
            | f 4 =
                (sub (vec, 4) << 0w1) orb ((sub (vec, 5) andb (word8 0wx80)) >> 0w7)
            | f 5 =
                (sub (vec, 5) << 0w1) orb ((sub (vec, 6) andb (word8 0wx80)) >> 0w7)
            | f 6 =
                (sub (vec, 6) << 0w1) orb carryRight
            | f _ = raise Fail "should never reach here"
        in
          Word8Vector.tabulate (7, f)
        end

  fun ror vec =
        let
          val sub = Word8Vector.sub
          val carryRight = (sub (vec, 6) andb (word8 0wx01)) << 0w3
          val carryLeft = (sub (vec, 3) andb (word8 0wx10)) << 0w3
          fun f 6 =
                (sub (vec, 6) >> 0w1) orb ((sub (vec, 5) andb (word8 0wx01)) << 0w7)
            | f 5 =
                (sub (vec, 5) >> 0w1) orb ((sub (vec, 4) andb (word8 0wx01)) << 0w7)
            | f 4 =
                (sub (vec, 4) >> 0w1) orb ((sub (vec, 3) andb (word8 0wx01)) << 0w7)
            | f 3 =
                (((sub (vec, 3) >> 0w1) orb ((sub (vec, 2) andb (word8 0wx01)) << 0w7)) andb notb (word8 0wx08)) orb carryRight
            | f 2 =
                (sub (vec, 2) >> 0w1) orb ((sub (vec, 1) andb (word8 0wx01)) << 0w7)
            | f 1 =
                (sub (vec, 1) >> 0w1) orb ((sub (vec, 0) andb (word8 0wx01)) << 0w7)
            | f 0 =
                (sub (vec, 0) >> 0w1) orb carryLeft
            | f _ = raise Fail "should never reach here"
        in
          Word8Vector.tabulate (7, f)
        end

  fun nextKeyEncrypt (1,  pc1Key56) = rol pc1Key56
    | nextKeyEncrypt (2,  pc1Key56) = rol pc1Key56
    | nextKeyEncrypt (9,  pc1Key56) = rol pc1Key56
    | nextKeyEncrypt (16, pc1Key56) = rol pc1Key56
    | nextKeyEncrypt (_,  pc1Key56) = rol (rol pc1Key56)

  fun nextKeyDecrypt (16, pc1Key56) = ror pc1Key56
    | nextKeyDecrypt (9,  pc1Key56) = ror pc1Key56
    | nextKeyDecrypt (2,  pc1Key56) = ror pc1Key56
    | nextKeyDecrypt (1,  pc1Key56) = pc1Key56
    | nextKeyDecrypt (_,  pc1Key56) = ror (ror pc1Key56)

  fun desBlockOperate nextKey (plainText64, key64) =
        let
          (* Initial permutation *)
          val ipBlock64 = permute (plainText64, ipTable)
          val leftHalf32 = Word8Vector.tabulate (4, fn i => Word8Vector.sub (ipBlock64, i))
          val rightHalf32 = Word8Vector.tabulate (4, fn i => Word8Vector.sub (ipBlock64, i + 4))
          val pc1Key56 = permute (key64, pc1Table)
          fun round (roundNum, leftHalf32, rightHalf32, pc1Key56) =
                if roundNum <= 16 then
                  let
                    (* 1. Expand bits 32-64 of the input to 48 bits. *)
                    val expansionBlock48 = permute (rightHalf32, expansionTable)

                    val pc1Key56 = nextKey (roundNum, pc1Key56)
                    val subKey48 = permute (pc1Key56, pc2Table)
                    (* 2. XOR the expanded right half of the input with the key. *)
                    val expansionBlock48 = xor (expansionBlock48, subKey48)

                    (* 3. Use the output of this XOR to look up eight entries in
                     *    the s-box table and overwrite the input with these
                     *    contents. *)
                    val substitutionBlock32 = Word8Vector.tabulate (4, fn i =>
                      let
                        val ih = i * 2
                        val il = ih + 1
                        val h = Vector.sub (Vector.sub (sbox, ih), Word.toInt (sliceBits (expansionBlock48, ih * 6, 6)))
                        val l = Vector.sub (Vector.sub (sbox, il), Word.toInt (sliceBits (expansionBlock48, il * 6, 6)))
                      in
                        Word8.orb (Word8.<< (Word8.fromInt h, 0w4), Word8.fromInt l)
                      end)

                    (* 4. Permute this output according to a specific p-table. *)
                    val pboxTarget32 = permute (substitutionBlock32, pTable)

                    (* 5. XOR this output with the left half of the input (bits
                     *    1-32) and swap sides so that the XORed left half
                     *    becomes the right half, and the (as of yet untouched)
                     *    right-half becomes the left half. On the next round,
                     *    the same series of operations are applied again, but
                     *    this time on what used to be the right half. *)
                    val recombBox32 = xor (pboxTarget32, leftHalf32)
                  in
                    round (roundNum + 1, rightHalf32, recombBox32, pc1Key56)
                  end
                else
                  Word8Vector.concat [rightHalf32, leftHalf32]
          val ipBlock64 = round (1, leftHalf32, rightHalf32, pc1Key56)
        in
          permute (ipBlock64, fpTable)
        end

  val desBlockEncrypt = desBlockOperate nextKeyEncrypt
  val desBlockDecrypt = desBlockOperate nextKeyDecrypt

  type instream = {
    src : BinStream.instream,
    input8 : BinStream.instream -> Word8Vector.vector * BinStream.instream,
    iv : Word8Vector.vector,
    key : Word8Vector.vector,
    prev : Word8Vector.vector,
    pad : Pad.pad,
    unpad : Pad.unpad,
    operate : (Word8Vector.vector * Word8Vector.vector * Word8Vector.vector) -> Word8Vector.vector
  }

  fun openString' operate (pad, unpad) (s, iv, key) =
        let
          val ins = TextIO.openString s
          val src = BinStream.fromFun (fn () => Byte.stringToBytes (TextIO.input ins))
          fun input8 ins =
                let
                  val (block, ins') = BinStream.inputN (ins, 8)
                in
                  (block, ins')
                end
        in
          { src = src,
            input8 = input8,
            iv = Byte.stringToBytes iv,
            key = Byte.stringToBytes key,
            prev = Byte.stringToBytes "",
            pad = pad,
            unpad = unpad,
            operate = operate }
        end

  fun openBytes' operate (pad, unpad) (bytes, iv, key) =
        openString' operate (pad, unpad) (Byte.bytesToString bytes, Byte.bytesToString iv, Byte.bytesToString key)

  fun input {src, input8, iv, key, prev, pad, unpad, operate} =
        let
          val (plainText, src') = input8 src
          val paddedText = pad (plainText, prev)
        in
          if Word8Vector.length paddedText = 0 then
              (plainText, {src=src', input8=input8, iv=plainText, key=key,
              prev=plainText, pad=pad, unpad=unpad, operate=operate})
          else
            let
              val block = operate (key, paddedText, iv)
              val (next, _) = input8 src'
              val block =
                if Word8Vector.length next = 0
                then unpad block
                else block
            in
              (block, {src=src', input8=input8, iv=block, key=key,
              prev=plainText, pad=pad, unpad=unpad, operate=operate})
            end
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

  structure Encrypt = struct
    type instream = instream

    val input = input
    val inputAll = inputAll

    val blockEncrypt = S.blockEncrypt (desBlockEncrypt, desBlockDecrypt)
    fun id x = x

    fun openString pad (s, iv, key) =
          openString' blockEncrypt (pad, id) (s, iv, key)
  
    fun openBytes pad (s, iv, key) =
          openBytes' blockEncrypt (pad, id) (s, iv, key)
  end

  structure Decrypt = struct
    type instream = instream

    val input = input
    val inputAll = inputAll

    val blockDecrypt = S.blockDecrypt (desBlockEncrypt, desBlockDecrypt)
    fun fst (x, _) = x

    fun openString unpad (s, iv, key) =
          openString' blockDecrypt (fst, unpad) (s, iv, key) 
  
    fun openBytes unpad (s, iv, key) =
          openBytes' blockDecrypt (fst, unpad) (s, iv, key)
  end

  fun encryptString pad (s, iv, key) =
        let
          val ins = Encrypt.openString pad (s, iv, key)
          val (vec, _) = inputAll ins
        in
          Byte.bytesToString vec
        end

  fun decryptString unpad (s, iv, key) =
        let
          val ins = Decrypt.openString unpad (s, iv, key)
          val (vec , _) = inputAll ins
        in
          Byte.bytesToString vec
        end
end

structure DES = DESFn(struct
  val xor = ExtWord8Vector.xor

  fun blockEncrypt (desBlockEncrypt, desBlockDecrypt) (key, block, v) =
        desBlockEncrypt (xor (block, v), key)

  fun blockDecrypt (desBlockEncrypt, desBlockDecrypt) (key, block, v) =
        xor (desBlockDecrypt (block, key), v)
end)

structure TripleDES = DESFn(struct
  val xor = ExtWord8Vector.xor

  fun splitKey key =
        let
          val slice = Word8VectorSlice.vector o Word8VectorSlice.slice
          val key1 = slice (key, 0, SOME 8)
          val key2 = slice (key, 8, SOME 16)
          val key3 = slice (key, 16, NONE)
        in
          (key1, key2, key3)
        end

  fun blockEncrypt (desBlockEncrypt, desBlockDecrypt) (key, block, v) =
        let
          val (key1, key2, key3) = splitKey key
          val fst = desBlockEncrypt (xor (block, v), key1)
          val snd = desBlockDecrypt (fst, key2)
        in
          desBlockEncrypt (snd, key3)
        end

  fun blockDecrypt (desBlockEncrypt, desBlockDecrypt) (key, block, v) =
        let
          val (key1, key2, key3) = splitKey key
          val fst = desBlockDecrypt (xor (block, v), key3)
          val snd = desBlockEncrypt (fst, key2)
        in
          xor (desBlockDecrypt (snd, key1), v)
        end
end)
