import abbsimple as abb
import time


def main():
    rob = abb.ABBRunner()  # defaults to 2530x2530 canvas
    rob.connectToSerial("/dev/tty.usbserial-AI03AZSZ")

    # TODO: Setup a default port in the main library too.

    time.sleep(3)
    rob.moveToSafe()
    time.sleep(3)
    rob.brushDown()
    time.sleep(3)
    rob.grip(0) # 0 is open, 1 is closed.
    time.sleep(3)

main()