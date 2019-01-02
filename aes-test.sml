structure AESTest = struct
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun testAesBlockEncrypt (plain, key) expected () =
        let
          val plain = Byte.stringToBytes plain
          val key = Byte.stringToBytes key
          val actual = ExtWord8Vector.bytesToHex (AESImpl.aesBlockEncrypt (plain, key))
        in
          Assert.assertEqualString expected actual
        end

  fun testBlockEncDec (plain, key) () =
        let
          val plain = Byte.stringToBytes plain
          val key = Byte.stringToBytes key
        in
          Assert.assertEqualString
            (ExtWord8Vector.bytesToHex plain)
            (ExtWord8Vector.bytesToHex (AESImpl.aesBlockDecrypt (AESImpl.aesBlockEncrypt (plain, key), key)))
        end

  fun testEncrypt128 plain expected () =
        let
          val encrypt = AES.encryptString (fn (x,y) => x)
          val stringToHex = ExtWord8Vector.bytesToHex o Byte.stringToBytes
          val actual = stringToHex (encrypt (plain, "initialzinitialz", "passwordpassword"));
        in
          Assert.assertEqualString expected actual
        end

  fun testEncrypt256 plain expected () =
        let
          val encrypt = AES.encryptString (fn (x,y) => x)
          val stringToHex = ExtWord8Vector.bytesToHex o Byte.stringToBytes
          val actual = stringToHex (encrypt (plain, "initialzinitialz", "passwordpasswordpasswordpassword"));
        in
          Assert.assertEqualString expected actual
        end

  val suite = Test.labelTests [
    ("aes-block-encrypt", testAesBlockEncrypt ("abcdefghabcdefgh", "0123456789abcdef")
     "ABF7D331A2387091A818CC931EEE47A1"),
    ("aes-block-encrypt", testAesBlockEncrypt ("abcdefghabcdefgh", "0123456789abcdef0123456789abcdef")
     "CE99C527E86E7E472AC429E368AAAEC3"),
    ("aes-block-encdec", testBlockEncDec ("abcdefghabcdefgh", "0123456789abcdef")),
    ("aes-block-encdec", testBlockEncDec ("abcdefghabcdefgh", "0123456789abcdef01234567")),
    ("aes-block-encdec", testBlockEncDec ("abcdefghabcdefgh", "0123456789abcdef0123456789abcdef")),
    ("aes-enc-128", testEncrypt128 "abcdefghabcdefgh" "EB4703AE3D8212C64C5A91CCC2C4078F"),
    ("aes-enc-128", testEncrypt128 "abcdefghabcdefghabcdefghabcdefgh" "EB4703AE3D8212C64C5A91CCC2C4078FC71E29A09F5D9A9A838F989B5EEAA866"),
    ("aes-enc-256", testEncrypt256 "abcdefghabcdefgh" "6826C134454ACD4F27988EC8F171913C"),
    ("aes-enc-256", testEncrypt256 "abcdefghabcdefghabcdefghabcdefgh" "6826C134454ACD4F27988EC8F171913C2B7385E133846C9007FBA38D5613984F"),
    ("placeholder", fn () => ())
  ]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = AESTest.run ()
