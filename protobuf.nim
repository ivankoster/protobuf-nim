type
  CodedOutputStream* = seq[byte]
  WireType* = enum
    Varint = 0,
    # Fixed64 = 1,
    # LengthDelimited = 2,
    # StartGroup = 3,
    # EndGroup = 4,
    # Fixed32 = 5

template MakeTag*(fieldNumber: int, wireType: WireType): uint =
  (uint)(fieldNumber shl 3) or (uint)wireType

template WriteTag*(this: var CodedOutputStream, fieldNumber: int, wireType: WireType) =
  let rawTag = MakeTag(fieldNumber, wireType)
  this.WriteRaw32BitsAsVarint(rawTag)

proc WriteRaw32BitsAsVarint*(this: var CodedOutputStream, value: uint) =
  var value = value
  while value >= 0b1000_0000'u:
    # add the 7 least significant bits and set bit 8 to indicate more bits will follow
    this.add((byte)(value and 0b0111_1111 or 0b1000_0000))
    value = value shr 7

  this.add((byte)value)

proc WriteRaw64BitsAsVarint(this: var CodedOutputStream, value: uint64) =
  assert(false)
  discard # todo: implement, we could even template it together with WriteRaw32BitsAsVarint

proc WriteInt32*(this: var CodedOutputStream, value: int) =
  if value >= 0:
    this.WriteRaw32BitsAsVarint((uint)value)
  else:
    # Google's wire format forces us to go 64 bits here, even though it's unnecessary
    # See https://groups.google.com/d/msg/protobuf/fzJfUNHVLJQ/cifCNTDT9xYJ
    this.WriteRaw64BitsAsVarint((uint64)value)

type
  CodedInputStream* = ref object
    buffer: seq[byte]
    offset: int

proc newCodedInputStream*(bytes: seq[byte]): CodedInputStream =
  CodedInputStream(buffer: bytes, offset: 0)

proc IsAtEnd(this: CodedInputStream): bool =
  this.buffer.len() == this.offset

proc ReadRawByte(this: CodedInputStream): byte =
  if this.IsAtEnd():
    assert(false)
  result = this.buffer[this.offset]
  inc(this.offset)

proc ReadVarintAsRaw32Bits(this: CodedInputStream): uint =
  result = 0'u
  var temp: uint

  template ReadAnotherByte(offset: int) =
    temp = this.ReadRawByte()
    if temp <= 0b0111_1111:
      return result or (temp shl offset)
    result = result or ((temp and 0b0111_1111) shl offset)

  ReadAnotherByte(0)
  ReadAnotherByte(1*7)
  ReadAnotherByte(2*7)
  ReadAnotherByte(3*7)

  # Byte 5 is special, it is supposed to be the last byte from an 32bit value encoded as varint
  temp = this.ReadRawByte()
  result = result or (temp shl 28)
  if temp > 0b0111_1111'u:
    # we got 64bit varint and we expect 32 bit, have to throw away the remaining bytes otherwise the buffer is broken
    assert(false)

proc ReadTag*(this: CodedInputStream): uint =
  if this.IsAtEnd():
    return 0

  result = this.ReadVarintAsRaw32Bits()
  if result == 0:
    assert(false) # 0 is not a valid tag

proc ReadInt32*(this: CodedInputStream): int =
  (int)this.ReadVarintAsRaw32Bits()