testType = {
  WRITE_BLOCK_AND_READ_AGAIN_PLAIN = 0x0,
  WRITE_BLOCK_AND_READ_AGAIN_CRC = 0x1,
  WRITE_DATASET_BLOCK_AND_READ_AGAIN = 0x2,
  WRITE_TO_LOCAL_RAM_MIRROR_AND_READ_AGAIN = 0x3,
  DETECT_CORRUPT_NVRAM_BLOCK = 0x4,
  RECOVERY_WITH_DEFAULT_DATA_NORMAL_BLOCK = 0x5,
  RECOVERY_WITH_DEFAULT_DATA_DATASET_BLOCK = 0x6,
  WRITE_BLOCK_AND_READ_AGAIN_SECRET = 0x7,
  WRITE_BLOCK_AND_READ_AGAIN_SECRET_WITH_DEFAULT_DATA = 0x8,
  --RECOVERY_FROM_REDUNDANT_BLOCK = 0x9,
}
function mkTests()
  local i = 1
	local tests = {}
	for k,v in pairs(testType) do
    tests[i] = DiagTest.new { name=k, send = Diag.new{0xbf,0x10,0x01,v}, expect = Diag.new{0xff,0x10,0x01}, timeout = 2000, source = 0xf4, target = 0x40}
  	i = i + 1
  end
  return tests
end

function runNvramTests(n)
  print("executing nvram tests")
  tests = mkTests()
  for k,v in pairs(tests) do
    print(v.name.." ==> "..DiagTest.tostring(v))
  end
  loop(n,tests)
end

