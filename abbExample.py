import abbsimple as abb
import code
import time

rob = abb.ABBRunner()  # defaults to 2530x2530 canvas

# Go to the Device manager to figure out which com port to actually use
rob.connectToSerial("COM9") # Initializes the code on the robot side
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

# opens a interactive console where the rob object still exists
# code.interact(local=locals()) 
