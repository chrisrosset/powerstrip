#!/usr/bin/env python
#
# powerstrip controller
# ver. 0.4

import serial
import sys
import os.path

import filelock

device = "/dev/ttyUSB0"

def help():
    return "usage: powerstrip [status/up/down] [0/1/all]"

def status(ser, port):
    thebits = [ "2", "5" ]
    mybits = thebits if port == "all" else [ thebits[int(port)] ]

    for bit in mybits:
        ser.write(bit)
        status = ser.read()
        print("off" if status == "0" else "on")

def power(ser, cmd, socket):
    thebits = {
        "down" : { "0" : "0", "1" : "3", "all" : "03" },
        "up"   : { "0" : "1", "1" : "4", "all" : "14" } }
    ser.write(thebits[cmd][socket])

def main(argv):
    funcs = {
        "up" : lambda ser, port : power(ser, "up", port),
        "down" : lambda ser, port : power(ser, "down", port),
        "status" : status
    }
    args = [ "0", "1", "all" ]

    lock = filelock.FileLock("/tmp/powerstrip-lock")

    try:
        if not os.path.exists(device):
            raise EnvironmentError("Serial link not detected.")

        if not (len(argv) == 2 and argv[0] in funcs and argv[1] in args):
            raise RuntimeError("Invalid arguments.")

        with lock.acquire(timeout = 10):
            ser = serial.Serial(device, 9600, timeout=1)
            funcs[argv[0]](ser, argv[1])
            ser.close()

    except filelock.Timeout, e:
        print "Failed to acquire file lock."
    except EnvironmentError, e:
        print(str(e))
    except RuntimeError, e:
        print(help())

if __name__ == "__main__":
    main(sys.argv[1:])
