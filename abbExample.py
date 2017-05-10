from abbsimple import ABBRunner
import code
import time

rob = ABBRunner(2530,2530)

# Go to the Device manager to figure out which com port to actually use
rob.connectToSerial("COM8")
rob.sendCanvasInfo() #Initializes the code on the robot side
time.sleep(3)
rob.moveToSafe()
time.sleep(2)

rob.moveTo(0,0)
time.sleep(1)
rob.brushDown()
time.sleep(1)
rob.moveTo(2530,0)
time.sleep(1)
rob.moveTo(2530,2530)
time.sleep(1)
rob.brushUp()
code.interact(local=locals())
