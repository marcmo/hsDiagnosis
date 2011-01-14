require "Lua/lunatest"
require "Lua/logging2"

ip_address="10.40.39.13"

loop(1,{
	DiagTest.new { name="TEST_DIAG_JOB", send=Diag.new{0x22,0xf1,0x90}, expect=Diag.new{0x63,"*"}, timeout=2000, source=0xf4, target=0x40 },
	DiagTest.new { name="WRITE_BLOCK_AND_READ_AGAIN_PLAIN", send=Diag.new{ 0xbf,0x10,0x1,0x1 }, expect=Diag.new{0xff,0x10,0x01}, timeout=2000, source=0xf4, target=0x40 },
	DiagTest.new { name="WRITE_BLOCK_AND_READ_AGAIN_CRC", send=Diag.new{0xbf,0x10,0x01,0x1}, expect=Diag.new{0xFF,0x10,0x01}, timeout=2000, source=0xF4, target=0x40}
	--DiagTest.new { name="WRITE_DATASET_BLOCK_AND_READ_AGAIN" SEND [bf,10,01,2] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]}
	--DiagTest.new { name="WRITE_TO_LOCAL_RAM_MIRROR_AND_READ_AGAIN" SEND [bf,10,01,3] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]}
	--DiagTest.new { name="DETECT_CORRUPT_NVRAM_BLOCK" SEND [bf,10,01,4] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]}
	--DiagTest.new { name="RECOVERY_WITH_DEFAULT_DATA_NORMAL_BLOCK" SEND [bf,10,01,5] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]}
	--DiagTest.new { name="RECOVERY_WITH_DEFAULT_DATA_DATASET_BLOCK" SEND [bf,10,01,6] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]}
	--DiagTest.new { name="WRITE_BLOCK_AND_READ_AGAIN_SECRET" SEND [bf,10,01,7] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]}
	--DiagTest.new { name="WRITE_BLOCK_AND_READ_AGAIN_SECRET_WITH_DEFAULT_DATA" SEND [bf,10,01,8] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]}
})
logger:log(logging.WARN, "try to sleep...")
sleep(200)
print("woke up again!")
sleep(200)
print("woke up again!")
sleep(200)
print("woke up again!")
sleep(200)
print("woke up again!")

