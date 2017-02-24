task build, "Buildx":
  switch("nimcache", "build/cache/")
  switch("out", "build/protobuf")
  setCommand "c"