import unittest
import protobuf
import math

suite "CodedOutputStream tests":
  setup:
    discard

  teardown:
    discard

  test "test WriteInt32 positive numbers":
    var actual, expected: CodedOutputStream
    actual = @[]
    actual.WriteInt32(0)
    expected = @[0b0000_0000'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteInt32(1)
    expected = @[0b0000_0001'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteInt32(127)
    expected = @[0b0111_1111'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteInt32(128)
    expected = @[0b1000_0000'u8, 0b0000_0001'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteInt32(high(int32))
    expected = @[0b11111111'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                 0b00000111'u8]
    check(expected == actual)

  test "test WriteInt32 negative numbers":
    echo "Todo: implement WriteInt32 negative numbers"

  test "test WriteTag":
    var actual, expected: CodedOutputStream
    actual = @[]
    actual.WriteTag(1, Varint)
    expected = @[8'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteTag(1, Fixed64)
    expected = @[9'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteTag(1, LengthDelimited)
    expected = @[10'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteTag(1, StartGroup)
    expected = @[11'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteTag(1, EndGroup)
    expected = @[12'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteTag(1, Fixed32)
    expected = @[13'u8]
    check(expected == actual)

    # max field number that fits in 1 byte
    actual = @[]
    actual.WriteTag(15, Varint)
    expected = @[0b0111_1000'u8]
    check(expected == actual)

    actual = @[]
    actual.WriteTag(15, Fixed32)
    expected = @[0b0111_1101'u8]
    check(expected == actual)

    # max field number that fits in 2 varint encoded bytes
    actual = @[]
    actual.WriteTag(2047, Varint)
    expected = @[0b1111_1000'u8, 0b0111_1111'u8]
    check(expected == actual)

    # max field number that fits in Int32 varint
    actual = @[]
    actual.WriteTag(2^29-1, Varint) #29 because 3 bits are for the wire type
    expected = @[0b11111000'u8, 0b11111111'u8, 0b11111111'u8, 0b11111111'u8,
                 0b00001111'u8]
    check(expected == actual)