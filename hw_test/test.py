#!/usr/bin/env -S python3 -u

import serial # type: ignore
import time
import statistics
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
def main():
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

    print("Test started")
    for addr in range(0x0000, 0x0010, 4):
        data = read(dut, addr)
        print(f"{addr:08x} : {data:08x}")

    for addr in range(0x1000, 0x1010, 4):
        data = read(dut, addr)
        print(f"{addr:08x} : {data:08x}")

    for addr in range(0x2000, 0x2008, 4):
        data = read(dut, addr)
        print(f"{addr:08x} : {data:08x}")

    for addr in range(0x3000, 0x3008, 4):
        data = read(dut, addr)
        print(f"{addr:08x} : {data:08x}")

    for addr in range(0x4000, 0x4008, 4):
        data = read(dut, addr)
        print(f"{addr:08x} : {data:08x}")

    # Read Temperature
    data = read(dut, 0x1000)
    temp = float(data) * 503.975 / 65536 - 273.15
    print(f"temp={temp:4.1f}")

    print("Test finished")

if __name__ == "__main__":
    main()

