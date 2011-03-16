require("base")
require("Diagnosis")

local ip = "10.40.39.19"
setIp(ip)

REDUCE_SIZE_OF_NVRAM_QUEUE = 0
REDUCE_SIZE_OF_NVRAM_EEPROM_QUEUE = 1
SET_CRC_SLOWDOWN = 2
DISABLE_EEPROM =3
ENABLE_EEPROM = 4
SET_ISR_DELAY = 5
RESET_QUEUES = 6

function mySend(msg)
  sendMsg(0xf4, 0x40, 5000, Diag.new(msg))
end
function reduceEepromQueue(n)
  mySend { 0xBF,0x12,0x06, REDUCE_SIZE_OF_NVRAM_EEPROM_QUEUE, n }
end
function reduceEmulationQueue(n)
  mySend { 0xBF,0x12,0x06, REDUCE_SIZE_OF_NVRAM_QUEUE, n }
end
function disableEeprom()
  mySend { 0xBF,0x12,0x06, DISABLE_EEPROM }
end
function enableEeprom()
  mySend { 0xBF,0x12,0x06, ENABLE_EEPROM }
end
function sysDelay(n)
  mySend { 0xBF,0x12,0x06, SET_ISR_DELAY, n }
end
function resetQueues()
  mySend { 0xBF,0x12,0x06, RESET_QUEUES }
end

reduceEepromQueue(5)
wait(1000)
reduceEmulationQueue(2)
wait(1000)
resetQueues()
wait(1000)
sysDelay(50)
wait(5000)
sysDelay(0)
wait(1000)
disableEeprom()
wait(1000)
enableEeprom()

