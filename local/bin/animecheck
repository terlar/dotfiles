#!/usr/bin/python
# Version 0.2 2009.01.25
# Copyright (c) 2009, Taoufik El Aoumari
# Released under the GPL license http://www.gnu.org/licenses/gpl-3.0.txt
 
import sys, re, zlib, os
 
c_null  = "\x1b[00;00m"
c_red   = "\x1b[31;01m"
c_green = "\x1b[32;01m"
p_reset = "\x08"*8
 
def crc32_checksum(filename):
    crc = 0
    file = open(filename, "rb")
    buff_size = 65536
    size = os.path.getsize(filename)
    done = 0
    try:
        while True:
            data = file.read(buff_size)
            done += buff_size
            sys.stdout.write("%7d"%(done*100/size) + "%" + p_reset)
            if not data:
                break
            crc = zlib.crc32(data, crc)
    except KeyboardInterrupt:
        sys.stdout.write(p_reset)
        file.close()
        sys.exit(1)
 
    sys.stdout.write(p_reset)
    file.close()
    if crc < 0:
        crc &= 2**32-1
    return "%.8X" %(crc)
 
for file in sys.argv[1:]:
    try:
        crc = crc32_checksum(file)
        dest_sum = re.split("([a-fA-F0-9]{8})", file)[-2]
        if crc == dest_sum.upper():
            c_in = c_green
        else:
            c_in = c_red
        sfile = file.split(dest_sum)
        print("%s%s%s   %s%s%s%s%s" % (c_in, crc, c_null, sfile[0], c_in, dest_sum, c_null, sfile[1]))
    except(IndexError, ValueError):
        print(crc, "   ", file)
    except (IOError, e):
        print(e)
        continue
