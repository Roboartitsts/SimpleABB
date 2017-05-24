# SimpleABB
A simple python class to connect to the ABB robot arm at Rose-Hulman.

The functions in this library will allow you to easily send movements commands to the Rose-Hulman ABB Arm through a serial connection.

## Usage
To use this library, you will need to have python installed on your computer, this library has been tested with python 3.x. To check the version of python you are running, use ```python --version``` in your command line.

This library uses serial communication for sending commands to the robot.  In order to use this, it is necessary to add the serial library to python.  To install the serial library, in your terminal use the command ```pip install pyserial``` or add pyserial as a dependency in your IDE.

In order to use the functions made available in this library, add the [abbsimple.py](/abbsimple.py) file to your project directory and type ```from abbsimple import ABBRunner``` at the top of your python file.

#### TL;DR
1. Use Python 3
2. ```pip install pyserial```
3. ```from abbsimple import ABBRunner```
4. ```rob = ABBRunner()```