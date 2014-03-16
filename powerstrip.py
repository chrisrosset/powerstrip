#!/usr/bin/env python
#
# powerstrip controller
# ver. 0.2

import serial
import sys
import os.path

def help():
    print "powerstrip terminal controller"
    print "usage: powerstrip status"
    print "       powerstrip [up/down] [0/1/all]"
    return 0

def status():
    if not os.path.exists(device):
        print "Serial link not detected."
        return 0

    ser = serial.Serial(device, 9600, timeout=1)
    ser.write("2")
    socket_zero = ser.read()
    ser.write("5")
    socket_one = ser.read()
    
    print "status:"
    print "0: socket zero (%s) is" % socket_zero_name,
    if socket_zero == "0":
        print "DOWN"
    else:
        print "UP"
    print "1: socket one  (%s) is" % socket_one_name,
    if socket_one == "0":
        print "DOWN"
    else:
        print "UP"

    ser.close()
    return 0

def power_up( socket ):
    ser = serial.Serial(device, 9600, timeout=1)

    if socket == "0":
        ser.write("2")
    elif socket == "1":
        ser.write("5")
    elif socket == "all":
        ser.write("14")
        print "All sockets powered UP."
        return 0

    status = ser.read()

    if socket == "0":
        socket_name = socket_zero_name
    elif socket == "1":
        socket_name = socket_one_name

    if status == "0":
        if socket == "0":
            # power up bit
            ser.write("1")
            # status bit
            ser.write("2")
        elif socket == "1":
            # power up bit
            ser.write("4")
            # status bit
            ser.write("5")

        status = ser.read()

        if status == "1":
            print "Socket " + socket + " (" + socket_name + ") powered UP."
        else:
            print "Powering up unsuccessful."

    else:
        print "Socket " + socket + " (" + socket_name + ") is already powered up."

    ser.close()
    return 0

def power_down( socket ):        
    ser = serial.Serial(device, 9600, timeout=1)

    if socket == "0":
        ser.write("2")
    elif socket == "1":
        ser.write("5")
    elif socket == "all":
        ser.write("03")
        print "All sockets powered DOWN."
        return 0

    x = ser.read()

    if socket == "0":
        socket_name = socket_zero_name
    elif socket == "1":
        socket_name = socket_one_name

    if x == "1":
        if socket == "0":
            ser.write("0")
        elif socket == "1":
            ser.write("3")

        print "Socket " + socket + " (" + socket_name + ") powered DOWN."
    else:
        print "Socket " + socket + " (" + socket_name + ") already powered down."

    ser.close()
    return 0

def main(argv):
    if len(argv) == 0:
	status()
    elif len(argv) == 1:
        if argv[0] == "status":
            status()
        else:
            help()
    elif len(argv) == 2:
        if argv[1] == "0" or argv[1] == "1" or argv[1] == "all":
            if argv[0] == "up":
                power_up(argv[1])
            elif argv[0] == "down":
                power_down(argv[1])
            else:
                #print "1st arg not up or down"
                help()
        else:
            #print "2nd arg not 0 or 1"
            help()
            
    else:
        #print "illegal no of args"
        help()

device = "/dev/ttyUSB0"
socket_zero_name = "Seagate External HDD"
socket_one_name = "iPhone AC Charger"
main(sys.argv[1:])
