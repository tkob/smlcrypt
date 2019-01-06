signature CRYPT = sig

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

end
