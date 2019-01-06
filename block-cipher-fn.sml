functor BlockCipherFn(S : sig
  val blockSize : int

  val blockEncrypt :
        (Word8Vector.vector * Word8Vector.vector * Word8Vector.vector)
        -> Word8Vector.vector

  val blockDecrypt :
        (Word8Vector.vector * Word8Vector.vector * Word8Vector.vector)
        -> Word8Vector.vector
end) :> CRYPT = struct
  val word8 = Word8.fromLarge o Word.toLarge
  val word = Word.fromLarge o Word8.toLarge
  val << = Word8.<<
  val >> = Word8.>>
  val orb = Word8.orb
  val andb = Word8.andb
  val notb = Word8.notb
  infix << >> orb andb

  type instream = {
    src : BinStream.instream,
    inputBlock : BinStream.instream -> Word8Vector.vector * BinStream.instream,
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
          fun inputBlock ins =
                let
                  val (block, ins') = BinStream.inputN (ins, S.blockSize)
                in
                  (block, ins')
                end
        in
          { src = src,
            inputBlock = inputBlock,
            iv = Byte.stringToBytes iv,
            key = Byte.stringToBytes key,
            prev = Byte.stringToBytes "",
            pad = pad,
            unpad = unpad,
            operate = operate }
        end

  fun openBytes' operate (pad, unpad) (bytes, iv, key) =
        openString' operate (pad, unpad) (Byte.bytesToString bytes, Byte.bytesToString iv, Byte.bytesToString key)

  fun input {src, inputBlock, iv, key, prev, pad, unpad, operate} =
        let
          val (plainText, src') = inputBlock src
          val paddedText = pad (plainText, prev)
        in
          if Word8Vector.length paddedText = 0 then
              (plainText, {src=src', inputBlock=inputBlock, iv=plainText, key=key,
              prev=plainText, pad=pad, unpad=unpad, operate=operate})
          else
            let
              val block = operate (key, paddedText, iv)
              val (next, _) = inputBlock src'
              val block =
                if Word8Vector.length next = 0
                then unpad block
                else block
            in
              (block, {src=src', inputBlock=inputBlock, iv=block, key=key,
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

    fun id x = x

    fun openString pad (s, iv, key) =
          openString' S.blockEncrypt (pad, id) (s, iv, key)

    fun openBytes pad (s, iv, key) =
          openBytes' S.blockEncrypt (pad, id) (s, iv, key)
  end

  structure Decrypt = struct
    type instream = instream

    val input = input
    val inputAll = inputAll

    fun fst (x, _) = x

    fun openString unpad (s, iv, key) =
          openString' S.blockDecrypt (fst, unpad) (s, iv, key)

    fun openBytes unpad (s, iv, key) =
          openBytes' S.blockDecrypt (fst, unpad) (s, iv, key)
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


