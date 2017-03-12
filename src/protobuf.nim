import macros

type
  CodedOutputStream* = seq[byte]
  WireType* = enum
    Varint = 0,
    Fixed64 = 1,
    LengthDelimited = 2,
    StartGroup = 3,
    EndGroup = 4,
    Fixed32 = 5
  PbType = enum
    PbInt32,
    PbInt64
    # todo more types..

type
  MalformedVarintError* = object of Exception
  TruncatedMessage* = object of Exception
  InvalidTag* = object of Exception

template MakeTag*(fieldNumber: int, wireType: WireType): uint =
  uint32(fieldNumber shl 3) or uint32(wireType)

proc GetWireType(tag: uint): WireType =
  WireType(tag and 0b0000_0111)

template WriteTag*(this: var CodedOutputStream, fieldNumber: int, wireType: WireType) =
  let rawTag = MakeTag(fieldNumber, wireType)
  this.WriteRaw32BitsAsVarint(rawTag)

proc WriteRaw32BitsAsVarint*(this: var CodedOutputStream, value: uint) =
  var value = value
  while value >= 0b1000_0000'u:
    # add the 7 least significant bits and set bit 8 to indicate more bits will follow
    this.add(byte(value and 0b0111_1111 or 0b1000_0000))
    value = value shr 7

  this.add(byte(value))

proc WriteRaw64BitsAsVarint(this: var CodedOutputStream, value: uint64) =
  assert(false)
  discard # todo: implement, we could even template it together with WriteRaw32BitsAsVarint

proc WriteInt32*(this: var CodedOutputStream, value: int) =
  if value >= 0:
    this.WriteRaw32BitsAsVarint(uint(value))
  else:
    # Google's wire format forces us to go 64 bits here, even though it's unnecessary
    # See https://groups.google.com/d/msg/protobuf/fzJfUNHVLJQ/cifCNTDT9xYJ
    this.WriteRaw64BitsAsVarint(uint64(value))

type
  CodedInputStream* = ref object
    buffer: seq[byte]
    offset: int

proc newCodedInputStream*(bytes: seq[byte]): CodedInputStream =
  CodedInputStream(buffer: bytes, offset: 0)

proc IsAtEnd*(this: CodedInputStream): bool =
  this.buffer.len() == this.offset

proc ReadRawByte(this: CodedInputStream): byte =
  if this.IsAtEnd():
    raise newException(TruncatedMessage, "The message in CodedInputStream is truncated")
  result = this.buffer[this.offset]
  inc(this.offset)

proc ReadVarintAsRaw32Bits(this: CodedInputStream): uint32 =
  result = 0'u32
  var temp: uint32

  template ReadAnotherByte(offset: int) =
    temp = this.ReadRawByte()
    if temp <= 0b0111_1111:
      return result or (temp shl offset)
    result = result or ((temp and 0b0111_1111) shl uint32(offset))

  ReadAnotherByte(0)
  ReadAnotherByte(1*7)
  ReadAnotherByte(2*7)
  ReadAnotherByte(3*7)

  # Byte 5 is special, it is supposed to be the last byte from an 32bit value encoded as varint
  temp = this.ReadRawByte()
  result = result or (temp shl 28)
  if temp >= 0b1000_0000'u:
    # we got 64bit varint and we expect 32 bit, have to throw away the remaining bytes otherwise the buffer is broken
    for i in 1..5:
      if this.ReadRawByte() < 0b1000_0000:
        return result
    raise newException(MalformedVarintError, "Varint encodes a value that is bigger than 64 bits")

proc ReadTag*(this: CodedInputStream): uint =
  if this.IsAtEnd():
    return 0

  result = this.ReadVarintAsRaw32Bits()
  if result == 0:
    raise newException(InvalidTag, "Read tag with value 0, which is not valid")

proc ReadInt32*(this: CodedInputStream): int32 =
  cast[int32](this.ReadVarintAsRaw32Bits())

proc SkipField*(this: CodedInputStream, tag: uint) =
  case tag.GetWireType():
  of Varint: discard this.ReadVarintAsRaw32Bits()
  of Fixed64: assert(false) # Not implemented yet
  of LengthDelimited: assert(false) # Not implemented yet
  of StartGroup: assert(false) # Not implemented yet
  of EndGroup: assert(false) # Not implemented yet
  of Fixed32: assert(false) # Not implemented yet

macro PbWriteTo*(obj: typed, output: CodedOutputStream, fieldInfo: untyped): untyped =
  expectKind(fieldInfo, nnkStmtList)
  result = newStmtList()

  for field in fieldInfo:
    expectKind(field, nnkCall)
    var fieldIdentifier = field[0]
    expectKind(field[1][0], nnkCommand)
    expectKind(field[1][0][1], nnkPrefix)
    var fieldNumberLiteral = field[1][0][1][1]
    result.add(newIfStmt((infix(newDotExpr(ident($obj.symbol), fieldIdentifier), "!=", newIntLitNode(0)),
                          newStmtList(newCall("WriteTag", ident($output[0].symbol), fieldNumberLiteral, ident("Varint")),
                                      newCall("WriteInt32", ident($output[0].symbol), newDotExpr(ident($obj.symbol), fieldIdentifier))))))

macro PbMergeFrom*(obj: typed, input: CodedInputStream, fieldInfo: untyped): untyped =
  expectKind(fieldInfo, nnkStmtList)

  var tagIdentifier = newIdentNode("tag")
  var caseBody = newTree(nnkCaseStmt, tagIdentifier)

  for field in fieldInfo:
    expectKind(field, nnkCall)
    var fieldIdentifier = field[0]
    expectKind(field[1][0], nnkCommand)
    expectKind(field[1][0][1], nnkPrefix)
    var fieldNumberLiteral = field[1][0][1][1]
    caseBody.add(newTree(nnkOfBranch,
                         newCall("MakeTag", fieldNumberLiteral, ident("Varint")),
                         newStmtList(newAssignment(newDotExpr(ident($obj[0].symbol), fieldIdentifier),
                                                   newCall("ReadInt32", input)
                                                  )
                                    )
                        )
                )
  # default case
  caseBody.add(newTree(nnkElse,
                       newStmtList(newCall("SkipField",
                                           input,
                                           tagIdentifier))))

  result = quote do:
    var `tagIdentifier`: uint
    while (`tagIdentifier` = input.ReadTag(); `tagIdentifier` != 0):
      `caseBody`