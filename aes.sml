structure AESImpl = struct
  val word8 = Word8.fromLarge o Word.toLarge
  val word = Word.fromLarge o Word8.toLarge
  val << = Word8.<<
  val >> = Word8.>>
  val orb = Word8.orb
  val andb = Word8.andb
  val xorb = Word8.xorb
  val notb = Word8.notb
  infix << >> orb andb xorb

  val sbox = Vector.fromList [
    Vector.fromList [ word8 0wx63, word8 0wx7c, word8 0wx77, word8 0wx7b,
                      word8 0wxf2, word8 0wx6b, word8 0wx6f, word8 0wxc5, 
                      word8 0wx30, word8 0wx01, word8 0wx67, word8 0wx2b,
                      word8 0wxfe, word8 0wxd7, word8 0wxab, word8 0wx76 ],
    Vector.fromList [ word8 0wxca, word8 0wx82, word8 0wxc9, word8 0wx7d,
                      word8 0wxfa, word8 0wx59, word8 0wx47, word8 0wxf0, 
                      word8 0wxad, word8 0wxd4, word8 0wxa2, word8 0wxaf,
                      word8 0wx9c, word8 0wxa4, word8 0wx72, word8 0wxc0 ],
    Vector.fromList [ word8 0wxb7, word8 0wxfd, word8 0wx93, word8 0wx26,
                      word8 0wx36, word8 0wx3f, word8 0wxf7, word8 0wxcc, 
                      word8 0wx34, word8 0wxa5, word8 0wxe5, word8 0wxf1,
                      word8 0wx71, word8 0wxd8, word8 0wx31, word8 0wx15 ],
    Vector.fromList [ word8 0wx04, word8 0wxc7, word8 0wx23, word8 0wxc3,
                      word8 0wx18, word8 0wx96, word8 0wx05, word8 0wx9a, 
                      word8 0wx07, word8 0wx12, word8 0wx80, word8 0wxe2,
                      word8 0wxeb, word8 0wx27, word8 0wxb2, word8 0wx75 ],
    Vector.fromList [ word8 0wx09, word8 0wx83, word8 0wx2c, word8 0wx1a,
                      word8 0wx1b, word8 0wx6e, word8 0wx5a, word8 0wxa0, 
                      word8 0wx52, word8 0wx3b, word8 0wxd6, word8 0wxb3,
                      word8 0wx29, word8 0wxe3, word8 0wx2f, word8 0wx84 ],
    Vector.fromList [ word8 0wx53, word8 0wxd1, word8 0wx00, word8 0wxed,
                      word8 0wx20, word8 0wxfc, word8 0wxb1, word8 0wx5b, 
                      word8 0wx6a, word8 0wxcb, word8 0wxbe, word8 0wx39,
                      word8 0wx4a, word8 0wx4c, word8 0wx58, word8 0wxcf ],
    Vector.fromList [ word8 0wxd0, word8 0wxef, word8 0wxaa, word8 0wxfb,
                      word8 0wx43, word8 0wx4d, word8 0wx33, word8 0wx85, 
                      word8 0wx45, word8 0wxf9, word8 0wx02, word8 0wx7f,
                      word8 0wx50, word8 0wx3c, word8 0wx9f, word8 0wxa8 ],
    Vector.fromList [ word8 0wx51, word8 0wxa3, word8 0wx40, word8 0wx8f,
                      word8 0wx92, word8 0wx9d, word8 0wx38, word8 0wxf5, 
                      word8 0wxbc, word8 0wxb6, word8 0wxda, word8 0wx21,
                      word8 0wx10, word8 0wxff, word8 0wxf3, word8 0wxd2 ],
    Vector.fromList [ word8 0wxcd, word8 0wx0c, word8 0wx13, word8 0wxec,
                      word8 0wx5f, word8 0wx97, word8 0wx44, word8 0wx17, 
                      word8 0wxc4, word8 0wxa7, word8 0wx7e, word8 0wx3d,
                      word8 0wx64, word8 0wx5d, word8 0wx19, word8 0wx73 ],
    Vector.fromList [ word8 0wx60, word8 0wx81, word8 0wx4f, word8 0wxdc,
                      word8 0wx22, word8 0wx2a, word8 0wx90, word8 0wx88, 
                      word8 0wx46, word8 0wxee, word8 0wxb8, word8 0wx14,
                      word8 0wxde, word8 0wx5e, word8 0wx0b, word8 0wxdb ],
    Vector.fromList [ word8 0wxe0, word8 0wx32, word8 0wx3a, word8 0wx0a,
                      word8 0wx49, word8 0wx06, word8 0wx24, word8 0wx5c, 
                      word8 0wxc2, word8 0wxd3, word8 0wxac, word8 0wx62,
                      word8 0wx91, word8 0wx95, word8 0wxe4, word8 0wx79 ],
    Vector.fromList [ word8 0wxe7, word8 0wxc8, word8 0wx37, word8 0wx6d,
                      word8 0wx8d, word8 0wxd5, word8 0wx4e, word8 0wxa9, 
                      word8 0wx6c, word8 0wx56, word8 0wxf4, word8 0wxea,
                      word8 0wx65, word8 0wx7a, word8 0wxae, word8 0wx08 ],
    Vector.fromList [ word8 0wxba, word8 0wx78, word8 0wx25, word8 0wx2e,
                      word8 0wx1c, word8 0wxa6, word8 0wxb4, word8 0wxc6, 
                      word8 0wxe8, word8 0wxdd, word8 0wx74, word8 0wx1f,
                      word8 0wx4b, word8 0wxbd, word8 0wx8b, word8 0wx8a ],
    Vector.fromList [ word8 0wx70, word8 0wx3e, word8 0wxb5, word8 0wx66,
                      word8 0wx48, word8 0wx03, word8 0wxf6, word8 0wx0e, 
                      word8 0wx61, word8 0wx35, word8 0wx57, word8 0wxb9,
                      word8 0wx86, word8 0wxc1, word8 0wx1d, word8 0wx9e ],
    Vector.fromList [ word8 0wxe1, word8 0wxf8, word8 0wx98, word8 0wx11,
                      word8 0wx69, word8 0wxd9, word8 0wx8e, word8 0wx94, 
                      word8 0wx9b, word8 0wx1e, word8 0wx87, word8 0wxe9,
                      word8 0wxce, word8 0wx55, word8 0wx28, word8 0wxdf ],
    Vector.fromList [ word8 0wx8c, word8 0wxa1, word8 0wx89, word8 0wx0d,
                      word8 0wxbf, word8 0wxe6, word8 0wx42, word8 0wx68, 
                      word8 0wx41, word8 0wx99, word8 0wx2d, word8 0wx0f,
                      word8 0wxb0, word8 0wx54, word8 0wxbb, word8 0wx16 ]
    ]

  val invSbox = Vector.fromList [
    Vector.fromList [ word8 0wx52, word8 0wx09, word8 0wx6a, word8 0wxd5,
                      word8 0wx30, word8 0wx36, word8 0wxa5, word8 0wx38,
                      word8 0wxbf, word8 0wx40, word8 0wxa3, word8 0wx9e,
                      word8 0wx81, word8 0wxf3, word8 0wxd7, word8 0wxfb ],
    Vector.fromList [ word8 0wx7c, word8 0wxe3, word8 0wx39, word8 0wx82,
                      word8 0wx9b, word8 0wx2f, word8 0wxff, word8 0wx87,
                      word8 0wx34, word8 0wx8e, word8 0wx43, word8 0wx44,
                      word8 0wxc4, word8 0wxde, word8 0wxe9, word8 0wxcb ],
    Vector.fromList [ word8 0wx54, word8 0wx7b, word8 0wx94, word8 0wx32,
                      word8 0wxa6, word8 0wxc2, word8 0wx23, word8 0wx3d,
                      word8 0wxee, word8 0wx4c, word8 0wx95, word8 0wx0b,
                      word8 0wx42, word8 0wxfa, word8 0wxc3, word8 0wx4e ],
    Vector.fromList [ word8 0wx08, word8 0wx2e, word8 0wxa1, word8 0wx66,
                      word8 0wx28, word8 0wxd9, word8 0wx24, word8 0wxb2,
                      word8 0wx76, word8 0wx5b, word8 0wxa2, word8 0wx49,
                      word8 0wx6d, word8 0wx8b, word8 0wxd1, word8 0wx25 ],
    Vector.fromList [ word8 0wx72, word8 0wxf8, word8 0wxf6, word8 0wx64,
                      word8 0wx86, word8 0wx68, word8 0wx98, word8 0wx16,
                      word8 0wxd4, word8 0wxa4, word8 0wx5c, word8 0wxcc,
                      word8 0wx5d, word8 0wx65, word8 0wxb6, word8 0wx92 ],
    Vector.fromList [ word8 0wx6c, word8 0wx70, word8 0wx48, word8 0wx50,
                      word8 0wxfd, word8 0wxed, word8 0wxb9, word8 0wxda,
                      word8 0wx5e, word8 0wx15, word8 0wx46, word8 0wx57,
                      word8 0wxa7, word8 0wx8d, word8 0wx9d, word8 0wx84 ],
    Vector.fromList [ word8 0wx90, word8 0wxd8, word8 0wxab, word8 0wx00,
                      word8 0wx8c, word8 0wxbc, word8 0wxd3, word8 0wx0a,
                      word8 0wxf7, word8 0wxe4, word8 0wx58, word8 0wx05,
                      word8 0wxb8, word8 0wxb3, word8 0wx45, word8 0wx06 ],
    Vector.fromList [ word8 0wxd0, word8 0wx2c, word8 0wx1e, word8 0wx8f,
                      word8 0wxca, word8 0wx3f, word8 0wx0f, word8 0wx02,
                      word8 0wxc1, word8 0wxaf, word8 0wxbd, word8 0wx03,
                      word8 0wx01, word8 0wx13, word8 0wx8a, word8 0wx6b ],
    Vector.fromList [ word8 0wx3a, word8 0wx91, word8 0wx11, word8 0wx41,
                      word8 0wx4f, word8 0wx67, word8 0wxdc, word8 0wxea,
                      word8 0wx97, word8 0wxf2, word8 0wxcf, word8 0wxce,
                      word8 0wxf0, word8 0wxb4, word8 0wxe6, word8 0wx73 ],
    Vector.fromList [ word8 0wx96, word8 0wxac, word8 0wx74, word8 0wx22,
                      word8 0wxe7, word8 0wxad, word8 0wx35, word8 0wx85,
                      word8 0wxe2, word8 0wxf9, word8 0wx37, word8 0wxe8,
                      word8 0wx1c, word8 0wx75, word8 0wxdf, word8 0wx6e ],
    Vector.fromList [ word8 0wx47, word8 0wxf1, word8 0wx1a, word8 0wx71,
                      word8 0wx1d, word8 0wx29, word8 0wxc5, word8 0wx89,
                      word8 0wx6f, word8 0wxb7, word8 0wx62, word8 0wx0e,
                      word8 0wxaa, word8 0wx18, word8 0wxbe, word8 0wx1b ],
    Vector.fromList [ word8 0wxfc, word8 0wx56, word8 0wx3e, word8 0wx4b,
                      word8 0wxc6, word8 0wxd2, word8 0wx79, word8 0wx20,
                      word8 0wx9a, word8 0wxdb, word8 0wxc0, word8 0wxfe,
                      word8 0wx78, word8 0wxcd, word8 0wx5a, word8 0wxf4 ],
    Vector.fromList [ word8 0wx1f, word8 0wxdd, word8 0wxa8, word8 0wx33,
                      word8 0wx88, word8 0wx07, word8 0wxc7, word8 0wx31,
                      word8 0wxb1, word8 0wx12, word8 0wx10, word8 0wx59,
                      word8 0wx27, word8 0wx80, word8 0wxec, word8 0wx5f ],
    Vector.fromList [ word8 0wx60, word8 0wx51, word8 0wx7f, word8 0wxa9,
                      word8 0wx19, word8 0wxb5, word8 0wx4a, word8 0wx0d,
                      word8 0wx2d, word8 0wxe5, word8 0wx7a, word8 0wx9f,
                      word8 0wx93, word8 0wxc9, word8 0wx9c, word8 0wxef ],
    Vector.fromList [ word8 0wxa0, word8 0wxe0, word8 0wx3b, word8 0wx4d,
                      word8 0wxae, word8 0wx2a, word8 0wxf5, word8 0wxb0,
                      word8 0wxc8, word8 0wxeb, word8 0wxbb, word8 0wx3c,
                      word8 0wx83, word8 0wx53, word8 0wx99, word8 0wx61 ],
    Vector.fromList [ word8 0wx17, word8 0wx2b, word8 0wx04, word8 0wx7e,
                      word8 0wxba, word8 0wx77, word8 0wxd6, word8 0wx26,
                      word8 0wxe1, word8 0wx69, word8 0wx14, word8 0wx63,
                      word8 0wx55, word8 0wx21, word8 0wx0c, word8 0wx7d ]
    ]

  fun subst (bytes, sbox) =
        let
          fun f b =
                let
                  val hi = Word8.toInt (b andb (word8 0wxf0) >> 0w4)
                  val lo = Word8.toInt (b andb (word8 0wx0f))
                in
                  Vector.sub (Vector.sub (sbox, hi), lo)
                end
        in
          Word8Vector.map f bytes
        end

  fun subBytes bytes = subst (bytes, sbox)
  fun invSubBytes bytes = subst (bytes, invSbox)

  val xor = ExtWord8Vector.xor
  val slice = Word8VectorSlice.vector o Word8VectorSlice.slice

  fun leftShift byte =
        let
          val word = word byte
          val shifted = Word.<< (word, 0w1)
        in
          (* if the left-shift overflows then ... *)
          if Word.andb (shifted, Word.notb 0wxff) <> 0w0 then
            word8 (Word.xorb (shifted, 0wx1b))
          else
            word8 shifted
        end

  fun nextRoundConstant roundConstant =
        let
          val firstByte = Word8Vector.sub (roundConstant, 0)
          fun f 0 = leftShift firstByte
            | f _ = word8 0wx00
        in
          Word8Vector.tabulate (4, f)
        end

  (*

  block size = 16 bytes

  key length    - # of rounds - keying material (+extra key permutation)
  128(16 bytes) - 10          - 160 bytes (176 bytes)
  192(24 bytes) - 12          - 192 bytes (208 bytes)
  256(32 bytes) - 14          - 224 bytes (240 bytes)

  rounds = (key-size in 4-byte words) + 6
  rounds = (key-length / 4) + 6

  keying material = 16 bytes for each round

  16-byte input -> AES key schedule computation -> 176 bytes of output
  fist 16 bytes = input itself
  remaining 160 bytes -> computed 4 at a time = permutation of the prev 4 bytes

  *)

  fun computeKeySchedule initialKey =
        let
          val keySize = Word8Vector.length initialKey
          val keyWords = keySize div 4
          val numRounds = keyWords + 6
          fun loop (n, roundConstant, history) =
                if n >= 4 * (numRounds + 1) then
                  let
                    fun aggregate ([], acc) = acc
                      | aggregate (k1::k2::k3::k4::keys, acc) =
                          aggregate (keys, Word8Vector.concat [k4, k3, k2, k1]::acc)
                      | aggregate (_, acc) =
                          raise Fail "should never reach here"
                  in
                    aggregate (history, [])
                  end
                else if n < keyWords then
                  let
                    val key = slice (initialKey, n * 4, SOME 4)
                  in
                    loop (n + 1, roundConstant, key::history)
                  end
                else
                  let
                    val nthLast = List.nth (history, keyWords - 1)
                    val last = List.hd history
                  in
                    if n mod keyWords = 0 then
                      let
                        val key = xor (nthLast, xor (roundConstant, subBytes (ExtWord8Vector.rotateLeft last)))
                      in
                        loop (n + 1, nextRoundConstant roundConstant, key::history)
                      end
                    else if keyWords > 6 andalso n mod keyWords = 4 then
                      let
                        val key = xor (nthLast, ExtWord8Vector.rotateLeft (subBytes last))
                      in
                        loop (n + 1, roundConstant, key::history)
                      end
                    else 
                      let
                        val key = xor (nthLast, last)
                      in
                        loop (n + 1, roundConstant, key::history)
                      end
                  end
        in
          loop (0, Byte.stringToBytes "\001\000\000\000", [])
        end

  structure State = struct
    (* a state represents 4x4 byte matrix *)
    datatype traversal = RowMajor | ColMajor
    type state = traversal * Word8Vector.vector

    fun transposedSub vec i =
          let
            val col = Int.quot (i, 4)
            val row = Int.rem (i, 4)
            val i = row * 4 + col
          in
            Word8Vector.sub (vec, i)
          end

    fun fromVector vec = (RowMajor, vec)
    fun toVector (RowMajor, vec) = vec
      | toVector (ColMajor, vec) =
          Word8Vector.tabulate (16, transposedSub vec)

    fun transpose (RowMajor, vec) = (ColMajor, vec)
      | transpose (ColMajor, vec) = (RowMajor, vec)

    fun sub ((RowMajor, vec), i, j) = Word8Vector.sub (vec, i * 4 + j)
      | sub ((ColMajor, vec), i, j) = Word8Vector.sub (vec, j * 4 + i)

    fun xor ((RowMajor, vec1), (RowMajor, vec2)) =
          (RowMajor, ExtWord8Vector.xor (vec1, vec2))
      | xor ((ColMajor, vec1), (ColMajor, vec2)) =
          (ColMajor, ExtWord8Vector.xor (vec1, vec2))
      | xor (s1 as (RowMajor, vec1), s2 as (ColMajor, vec2)) =
          let
            fun f i =
                  Word8Vector.sub (vec1, i) xorb transposedSub vec2 i
            val vec = Word8Vector.tabulate (16, f)
          in
            (RowMajor, vec)
          end
      | xor (s1 as (ColMajor, vec1), s2 as (RowMajor, vec2)) =
          xor (s2, s1)

    fun subst (traversal, vec) = (traversal, subBytes vec)
    fun invSubst (traversal, vec) = (traversal, invSubBytes vec)

    fun shiftRows s =
          let
            fun f i =
                  let
                    val row = Int.quot (i, 4)
                  in
                    sub (s, row, Int.rem (i + row, 4))
                  end
          in
            (RowMajor, Word8Vector.tabulate (16, f))
          end

    fun invShiftRows s =
          let
            fun f i =
                  let
                    val row = Int.quot (i, 4)
                  in
                    sub (s, row, Int.rem (i - row + 4, 4))
                  end
          in
            (RowMajor, Word8Vector.tabulate (16, f))
          end

    fun dot (x, y) =
          let
            fun loop (0wx00, x, product) = product
              | loop (mask, x, product) =
                  loop (
                    mask << 0w1,
                    leftShift x,
                    if (y andb mask) <> 0wx00 then product xorb x else product)
          in
            loop (0wx01, x, 0wx00)
          end

    fun multiply (op <+>, op <*>) (s1, s2) =
          let
            infix <+> <*>
            fun f i =
                  let
                    val row = Int.quot (i, 4)
                    val col = Int.rem (i, 4)
                  in
                    (sub (s1, row, 0) <*> sub (s2, 0, col))
                    <+>
                    (sub (s1, row, 1) <*> sub (s2, 1, col))
                    <+>
                    (sub (s1, row, 2) <*> sub (s2, 2, col))
                    <+>
                    (sub (s1, row, 3) <*> sub (s2, 3, col))
                  end
          in
            (RowMajor, Word8Vector.tabulate (16, f))
          end

    val matrix =
      fromVector (Byte.stringToBytes
        ("\002\003\001\001" ^
         "\001\002\003\001" ^
         "\001\001\002\003" ^
         "\003\001\001\002"))

    val invMatrix =
      fromVector (Byte.stringToBytes
        ("\014\011\013\009" ^
         "\009\014\011\013" ^
         "\013\009\014\011" ^
         "\011\013\009\014"))

    fun mixColumns s = multiply (Word8.xorb, dot) (matrix, s)
    fun invMixColumns s = multiply (Word8.xorb, dot) (invMatrix, s)
  end

  fun addRoundKey (state, w) =
        State.xor (state, State.transpose w)

  fun aesBlockEncrypt (input, key) =
        let
          fun loop (state, []) = State.toVector (State.transpose state)
            | loop (state, key::[]) =
                let
                  val state = State.shiftRows (State.subst state)
                  val state = addRoundKey (state, State.fromVector key)
                in
                  loop (state, [])
                end
            | loop (state, key::scheduledKeys) =
                let
                  val state = State.shiftRows (State.subst state)
                  val state = State.mixColumns state
                  val state = addRoundKey (state, State.fromVector key)
                in
                  loop (state, scheduledKeys)
                end
          val (key::scheduledKeys) = computeKeySchedule key
          val state =
            addRoundKey (State.transpose (State.fromVector input), State.fromVector key)
        in
          loop (state, scheduledKeys)
        end

  fun aesBlockDecrypt (input, key) =
        let
          fun loop (state, []) = State.toVector (State.transpose state)
            | loop (state, key::[]) =
                let
                  val state = State.invSubst (State.invShiftRows state)
                  val state = addRoundKey (state, State.fromVector key)
                in
                  loop (state, [])
                end
            | loop (state, key::scheduledKeys) =
                let
                  val state = State.invSubst (State.invShiftRows state)
                  val state = addRoundKey (state, State.fromVector key)
                  val state = State.invMixColumns state
                in
                  loop (state, scheduledKeys)
                end
          val (key::scheduledKeys) = rev (computeKeySchedule key)
          val state =
            addRoundKey (State.transpose (State.fromVector input), State.fromVector key)
        in
          loop (state, scheduledKeys)
        end

end

structure AES = BlockCipherFn(struct
  open AESImpl

  val blockSize = 16

  fun blockEncrypt (key, block, v) =
        aesBlockEncrypt (xor (block, v), key)

  fun blockDecrypt (key, block, v) =
        xor (aesBlockDecrypt (block, key), v)
end)
