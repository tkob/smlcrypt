structure RSATest = struct
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun testRsaCompute (m, e, n, c) () =
          Assert.assertEqual (op =) IntInf.toString (RSA.rsaCompute (IntInf.fromInt m, IntInf.fromInt e, IntInf.fromInt n)) (IntInf.fromInt c)

  val word8 = Word8.fromLarge o Word.toLarge

  val testModulus = Word8Vector.fromList [
    word8 0wxc4, word8 0wxf8, word8 0wxe9, word8 0wxe1, word8 0wx5d, word8 0wxca, word8 0wxdf, word8 0wx2b,
    word8 0wx96, word8 0wxc7, word8 0wx63, word8 0wxd9, word8 0wx81, word8 0wx00, word8 0wx6a, word8 0wx64,
    word8 0wx4f, word8 0wxfb, word8 0wx44, word8 0wx15, word8 0wx03, word8 0wx0a, word8 0wx16, word8 0wxed,
    word8 0wx12, word8 0wx83, word8 0wx88, word8 0wx33, word8 0wx40, word8 0wxf2, word8 0wxaa, word8 0wx0e,
    word8 0wx2b, word8 0wxe2, word8 0wxbe, word8 0wx8f, word8 0wxa6, word8 0wx01, word8 0wx50, word8 0wxb9,
    word8 0wx04, word8 0wx69, word8 0wx65, word8 0wx83, word8 0wx7c, word8 0wx3e, word8 0wx7d, word8 0wx15,
    word8 0wx1b, word8 0wx7d, word8 0wxe2, word8 0wx37, word8 0wxeb, word8 0wxb9, word8 0wx57, word8 0wxc2,
    word8 0wx06, word8 0wx63, word8 0wx89, word8 0wx82, word8 0wx50, word8 0wx70, word8 0wx3b, word8 0wx3f]

  val testPrivateKey = Word8Vector.fromList [
    word8 0wx8a, word8 0wx7e, word8 0wx79, word8 0wxf3, word8 0wxfb, word8 0wxfe, word8 0wxa8, word8 0wxeb,
    word8 0wxfd, word8 0wx18, word8 0wx35, word8 0wx1c, word8 0wxb9, word8 0wx97, word8 0wx91, word8 0wx36,
    word8 0wxf7, word8 0wx05, word8 0wxb4, word8 0wxd9, word8 0wx11, word8 0wx4a, word8 0wx06, word8 0wxd4,
    word8 0wxaa, word8 0wx2f, word8 0wxd1, word8 0wx94, word8 0wx38, word8 0wx16, word8 0wx67, word8 0wx7a,
    word8 0wx53, word8 0wx74, word8 0wx66, word8 0wx18, word8 0wx46, word8 0wxa3, word8 0wx0c, word8 0wx45,
    word8 0wxb3, word8 0wx0a, word8 0wx02, word8 0wx4b, word8 0wx4d, word8 0wx22, word8 0wxb1, word8 0wx5a,
    word8 0wxb3, word8 0wx23, word8 0wx62, word8 0wx2b, word8 0wx2d, word8 0wxe4, word8 0wx7b, word8 0wxa2,
    word8 0wx91, word8 0wx15, word8 0wxf0, word8 0wx6e, word8 0wxe4, word8 0wx2c, word8 0wx41]

  val testPublicKey = Word8Vector.fromList [word8 0wx01, word8 0wx00, word8 0wx01]

  val testPrivateKey = { modulus = testModulus, exponent = testPrivateKey }
  val testPublicKey = { modulus = testModulus, exponent = testPublicKey }

  fun testRsaBlockEncrypt plain expected () =
        let
          val encrypted = Byte.stringToBytes (RSA.encryptString (plain, testPublicKey))
        in
          Assert.assertEqualString expected (ExtWord8Vector.bytesToHex encrypted)
        end

  fun testRsaEncryptDecrypt plain () =
        let
          val encrypted = (RSA.encryptString (plain, testPublicKey))
          val decrypted = (RSA.decryptString (encrypted, testPrivateKey))
        in
          Assert.assertEqualString plain decrypted
        end

  val suite = Test.labelTests [
    ("rsa encrypt", testRsaCompute (688, 79, 3337, 1570)),
    ("rsa decrypt", testRsaCompute (1570, 1019, 3337, 688)),
    ("rsa block decrypt", testRsaBlockEncrypt "abc"
     ("40F73315D3F74703904E51E1C72686801DE06A55417110E56280F1F8471A3802406D2110011E1F38"
     ^ "7F7B4C43258B0A1EEDC558A3AAC5AA2D20CF5E0D65D80DB3")),
    ("rsa encrypt-decrypt", testRsaEncryptDecrypt "abc"),
    ("rsa encrypt-decrypt", testRsaEncryptDecrypt "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"),
    ("placeholder", fn () => ())
  ]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = RSATest.run ()
