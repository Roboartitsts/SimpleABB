import serial
import time
class ABBRunner():

	def __init__(self, width, height):
		self.ser = None
		self.connected = False
		self.width = width
		self.height = height

	def moveTo(self, x, y):
		if not self.connected:
			return False

		msg = "COORDM:X:" + str(x) + ",Y:" + str(y) + ";"
		return self.sendSerial(msg)

	def sendBezierPoints(self, x1, y1, x2, y2 ,x3, y3):
		if not self.connected:
			return False

		msg = "COORDQ:X:{:.3f},Y:{:.3f}#X:{:.3f},Y:{:.3f}#X:{:.3f},Y:{:.3f};".format(x1,y1,x2,y2,x3,y3)
		return self.sendSerial(msg)

	def gripToggle(self):
		''' By default toggles if '''
		msg = "GRIP;"
		return self.sendSerial(msg)

	def brushUp(self):
		''' By default toggles if '''
		msg = "BRUSHUP;"
		return self.sendSerial(msg)

	def brushDown(self):
		''' By default toggles if '''
		msg = "BRUSHDOWN;"
		return self.sendSerial(msg)

	def gripOpen(self):
		msg = "GRIPOPEN;"
		return self.sendSerial(msg)

	def gripClose(self):
		msg = "GRIPCLOSE;"
		return self.sendSerial(msg)
		
	def moveToSafe(self,):
		if not self.connected:
			return False
		return self.sendSerial("MOVETOSAFE;")

	def sendCanvasInfo(self):
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
		self.width = width
		self.height = height

	def waitRobotReady(self):
		while True:
			code = self.readSerialLine()
			print("waiting on robot...")
			if code:
				print(code)
				return True

		return False

	def abort(self,):
		self.ser.close()

	def connectToSerial(self, port):
		self.ser = serial.Serial(port, 115200, timeout=0.1)
		self.connected = True

	def readSerial(self,):
		msg = self.ser.read(2)
		return msg

	def readSerialLine(self,):
		msg = self.ser.readline()
		return msg

	def sendSerial(self, msg):
		if self.connected == False:
			return False
		print(msg)
		# TODO: Try/catch?
		self.ser.write(msg.encode())
                #print('waiting for response')
		resp = self.ser.read(1)
		# print("received command:",resp)
		#print('---respfast----')
                #if resp != '':
                #    print(resp,ord(resp))
		#print('---respfast----')
		while resp == '':
			time.sleep(0.1)
			resp = self.ser.read(1)
			if ord(resp) == 2:
    				return self.sendSerial(msg)
		return True
