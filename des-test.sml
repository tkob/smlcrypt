structure DESTest = struct
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun testEncrypt plain expected () =
        let
          val encrypt = DES.encryptString Pad.padNist;
          val stringToHex = ExtWord8Vector.bytesToHex o Byte.stringToBytes
          val actual = stringToHex (encrypt (plain, "initialz", "password"));
        in
          Assert.assertEqualString expected actual
        end

  fun testEncrypt3 plain expected () =
        let
          val encrypt = TripleDES.encryptString Pad.padNist;
          val stringToHex = ExtWord8Vector.bytesToHex o Byte.stringToBytes
          val actual = stringToHex (encrypt (plain, "initialz", "twentyfourcharacterinput"));
        in
          Assert.assertEqualString expected actual
        end

  val suite = Test.labelTests [
    ("encrypt eight bytes", testEncrypt "abcdefgh" "71828547387B18E5"),
    ("encrypt sixteen bytes", testEncrypt "abcdefghijklmnop" "71828547387B18E50EFB4A2513489345"),
    ("encrypt six bytes", testEncrypt "abcdef" "70F220F6A39DE6AD"),
    ("encrypt six bytes with ambiguos suffix", testEncrypt "abcdef\128\000" "70F220F6A39DE6AD7C8ADD7129FC600D"),
    ("encrypt ambiguous eight bytes", testEncrypt "\128\000\000\000\000\000\000\000" "101008ED149B97B94523DC3ADEE46CAB"),
    ("tripledes-encrypt eight bytes", testEncrypt3 "abcdefgh" "C0C48BC47E87CE17"),
    ("placeholder", fn () => ())
  ]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = DESTest.run ()
