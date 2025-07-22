#!/usr/bin/env -S python3 -u

import serial # type: ignore
import time
import statistics
import sys
from typing import List
from typing import Optional

# Helper functions
def int2ba(a : int) -> bytearray:
    b = bytearray()
    s = f'{a:08x}'
    b.extend(s.encode())
    return b

def ba2int(b : bytearray) -> int:
    s = b.decode('ascii').strip().strip('\x00')
    return int(s, 16)

def write(seri : serial.Serial, addr : int, data : int):
    seri.reset_input_buffer()
    msg = b'W ' + int2ba(addr) + b' ' + int2ba(data) + b'\x0d' + b'\x0a'
    seri.write(msg)
    res = seri.readline() # Discard result

def read(seri : serial.Serial, addr : int) -> int:
    seri.reset_input_buffer()
    res = seri.readline() # Discard result
    msg = b'R ' + int2ba(addr) + b'\x0d' + b'\x0a'
    seri.write(msg)
    res = seri.readline()
    return ba2int(res)

# Main test program
def main(argv):
    if len(argv) < 2:
        print("Usage: cmd <addres> [ <data> ]")
        sys.exit(-1)

    dut = serial.Serial(
        port='/dev/ttyUSB1',
        baudrate=115200,
        timeout=0.1,
        parity=serial.PARITY_NONE,
        stopbits=serial.STOPBITS_ONE,
        bytesize=serial.EIGHTBITS
    )

    dut.reset_input_buffer()
    # Discard any initial output
    while len(dut.readline()) != 0:
        pass

    if len(argv) == 2:
        addr = int(argv[1], 16)
        data = read(dut, addr)
        print(f"read({addr:04x}) = {data:04x}")

    if len(argv) == 3:
        addr = int(argv[1], 16)
        data = int(argv[2], 16)
        write(dut, addr, data)
        print(f"write({addr:04x}) = {data:04x}")

if __name__ == "__main__":
    main(sys.argv)

