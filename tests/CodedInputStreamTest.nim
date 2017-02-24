import unittest
import protobuf
import math

suite "CodedInputStream tests":
  setup:
    discard

  teardown:
    discard

  test "test ReadInt32 positive numbers":
    var input = newCodedInputStream(@[0b0000_0000'u8])
    var expected = 0
    check(expected == input.ReadInt32())

    input = newCodedInputStream(@[0b0111_1111'u8])
    expected = 127
    check(expected == input.ReadInt32())

    input = newCodedInputStream(@[0b1000_0000'u8, 0b0000_0001'u8])
    expected = 128
    check(expected == input.ReadInt32())

    input = newCodedInputStream(@[0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b00000111'u8])
    expected = high(int32)
    check(expected == input.ReadInt32())

  test "test ReadInt32 with Int64 in stream":
    # The wiretype Varint can be 32bit or 64bit integers
    # A client can call ReadInt32 while there is a 64 bit integer in the stream
    # The superfluous bits must be read from the stream, otherwise it gets corrupted
    var input: CodedInputStream

    # First test if just a single extra bit is simply discarded
    var expected = 0
    input = newCodedInputStream(@[0b10000000'u8, 0b10000000'u8, 0b10000000'u8, 0b10000000'u8,
                                  0b00010000'u8])
    check(expected == input.ReadInt32())
    check(input.IsAtEnd())

    expected = -1
    input = newCodedInputStream(@[0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b00011111'u8])
    check(expected == input.ReadInt32())
    check(input.IsAtEnd())

    # Test that 1 extra byte is thrown away
    expected = -1
    input = newCodedInputStream(@[0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b11111111'u8, 0b00000001])
    check(expected == input.ReadInt32())
    check(input.IsAtEnd())

    # Test that 5 extra bytes are thrown away
    expected = -1
    input = newCodedInputStream(@[0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b11111111'u8, 0b00000101'u8])
    check(expected == input.ReadInt32())
    check(input.IsAtEnd())

    # Test that 6 extra bytes throws error
    input = newCodedInputStream(@[0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b11111111'u8, 0b10000101'u8, 0b00000101'u8])
    expect MalformedVarintError:
      discard input.ReadInt32()

  test "test ReadInt32 negative numbers":
    echo "Todo: implement ReadInt32 negative numbers"

  test "test ReadTag":
    var input: CodedInputStream
    var expected: uint

    expected = 0
    input = newCodedInputStream(@[])
    check(expected == input.ReadTag())

    expected = 8
    input = newCodedInputStream(@[8'u8])
    check(expected == input.ReadTag())

    # max field number that fits in 1 byte
    expected = 15 shl 3 + int(Fixed32)
    input = newCodedInputStream(@[0b0111_1101'u8])
    check(expected == input.ReadTag())

    # max field number that fits in 2 varint encoded bytes
    expected = 2047 shl 3 + int(Varint)
    input = newCodedInputStream(@[0b1111_1000'u8, 0b0111_1111'u8])
    check(expected == input.ReadTag())

    # max field number that fits in Int32 varint
    expected = uint(2^29-1) shl 3'u + uint(Varint) #29 because 3 bits are for the wire type
    input = newCodedInputStream(@[0b11111000'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                                  0b00001111'u8])
    check(expected == input.ReadTag())