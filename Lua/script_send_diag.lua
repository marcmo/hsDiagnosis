require("base")

setIp("10.40.39.19")

require("nvramtests")
runNvramTests(2);



print("here")
logger:log(logging.WARN, "try to wait...")
wait(200)
print("woke up again!")
wait(200)
print("up")
wait(200)
print("again!")

