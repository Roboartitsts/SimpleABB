import serial
import time
class ABBRunner():

    def __init__(self, width=2530, height=2530):
        self.ser = None
        self.connected = False
        self.width = width
        self.height = height

    def moveTo(self, x, y):
        """
        Moves the robot arm to the specified (x,y) position on the canvas.

        Parameters
        ----------
        x : int
            X Position of the destination point

        y : int
            Y Position of the destination point

        """
        if not self.connected:
            return False

        msg = "COORDM:X:" + str(x) + ",Y:" + str(y) + ";"
        return self.sendSerial(msg)

    def sendBezierPoints(self, x1, y1, x2, y2 ,x3, y3):
        """
        Moves the robot arm along the specified Bezier Curve.

        Parameters
        ----------
        x : int
            X Position of the destination point

        y : int
            Y Position of the destination point

        """
        if not self.connected:
            return False

        msg = "COORDQ:X:{:.3f},Y:{:.3f}#X:{:.3f},Y:{:.3f}#X:{:.3f},Y:{:.3f};".format(x1, y1, x2, y2, x3, y3)
        return self.sendSerial(msg)

    def gripToggle(self):
        """
        Toggles the ABB Gripper
        """
        msg = "GRIP;"
        return self.sendSerial(msg)

    def brushUp(self):
        """
        Takes the Brush off the canvas
        """
        msg = "BRUSHUP;"
        return self.sendSerial(msg)

    def brushDown(self):
        """
        Places the Brush on the canvas
        """
        msg = "BRUSHDOWN;"
        return self.sendSerial(msg)

    def grip(self, toSqueeze):
        """
        Allows the user to specify the configuration of the gripper

        Parameters
        ----------
        toSqueeze : int

            1 to grip
            0 to release

        """
        if toSqueeze:
            msg = "GRIPV" + str(toSqueeze)+ ";"
        return self.sendSerial(msg)

    def moveToSafe(self,):
        """
        moves the ABB Arm to a safe position above the canvas
        """
        if not self.connected:
            return False
        return self.sendSerial("MOVETOSAFE;")

    def sendCanvasInfo(self):
        """
        sends the canvas info to the abb arm
        """
        if not self.connected:
            return False

        # Wait for robot to be ready
        if not self.waitRobotReady():
            return False
        # self.ser.timeout = None

        msg = "SIZE:X:" + str(self.width) + ",Y:" + str(self.height) + ";"
        self.sendSerial(msg)
        something = self.readSerial()
        self.moveToSafe()
        return something

    def setSize(self, width, height):
        """
        sets the size of the drawing area available to the robot
        """
        self.width = width
        self.height = height

    def waitRobotReady(self):
        """
        waits until the ABB Arm is able to receive serial communication
        """
        while True:
            code = self.readSerialLine()
            print("waiting on robot...")
            if code:
                print(code)
                return True

        return False

    def abort(self,):
        """
        closes serial communication
        """
        self.ser.close()

    def connectToSerial(self, port):
        """
        connects to the specified serial port

        Parameters
        ----------
        port : str

            The com port that the robot is connected to
            On a Mac/Unix machine this will be of the form '/dev/<usbSerialdevice>'
            On a Windows machine this will be 'COM#'
        """
        self.ser = serial.Serial(port, 115200, timeout=0.1)
        self.connected = True
        self.sendCanvasInfo()

    def readSerial(self,):
        """
        reads a serial message from the ABB Arm
        """
        msg = self.ser.read(2)
        return msg

    def readSerialLine(self,):
        """
        Reads a line of serial communication from the ABB Arm
        """
        msg = self.ser.readline()
        return msg

    def sendSerial(self, msg):
        """
        Sends a command to the robot over the serial connection
        """
        if not self.connected:
            return False
        print(msg)
        self.ser.write(msg.encode())
        resp = self.ser.read(1)
        while resp == '':
            time.sleep(0.1)
            resp = self.ser.read(1)
            if ord(resp) == 2:
                return self.sendSerial(msg)
        return True
