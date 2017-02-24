task build, "Building":
  switch("nimcache", "build/cache/")
  switch("out", "build/protobuf")
  setCommand "c"

task tests, "Testing":
  selfExec "c -r --nimcache:build/cache/ --out:build/CodedOutputStreamTest tests/CodedOutputStreamTest"
  selfExec "c -r --nimcache:build/cache/ --out:build/CodedInputStreamTest tests/CodedInputStreamTest"
  setCommand "nop"