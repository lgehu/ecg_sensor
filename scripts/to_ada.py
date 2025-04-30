#!/usr/bin/env python3
import sys
import os
import argparse
import wfdb
import struct

def generate_ada_array(input_file, output_file, isWFDB, symbol_name="Data"):
    
    data = []
    array_type = "Interfaces.IEEE_Float_32" if isWFDB else "Interfaces.Unsigned_8"
    sample_rate = 0
    if isWFDB:
        signals, fields = wfdb.rdsamp(input_file, channels=[0])
        sample_rate = fields['fs']
        print(fields)
        for sig in signals:
            data.append(sig[0])
            #bb = struct.pack('>h', int(sig[0] * 1000))
            #data.append(bb[0])
            #data.append(bb[1])
    else:
        with open(input_file, "rb") as f:
            data = f.read()

    # Open the Ada output file
    with open(output_file, "w") as f:
        basename = os.path.basename(input_file).replace('.', '_')
        
        f.write("with Interfaces;\n")
        if isWFDB:
            f.write('use type Interfaces.IEEE_Float_32;\n')

        f.write(f'-- This file was generated with {os.path.basename(__file__)}\n')
        f.write(f"-- File from {input_file}\n")
        f.write(f"package {symbol_name} is\n")

        if isWFDB:
            f.write(f"   Sample_Rate : constant := {int(sample_rate)};\n")

        f.write(f"   Data : constant array (1 .. {len(data)}) of {array_type} := (\n")

        # Write the binary content in Ada array form
        for idx, byte in enumerate(data, start=1):
            if idx % 12 == 1:
                f.write("      ")  # indentation
            f.write(f"{byte}")

            if idx != len(data):
                f.write(", ")
                if idx % 12 == 0:
                    f.write("\n")
            else:
                f.write("\n")

        f.write("   );\n")
        f.write(f"   Data_Size : constant := {len(data)};\n")
        f.write(f"end {symbol_name};\n")

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Convert any file into an Ada array")

    parser.add_argument("input", help="Input file")
    parser.add_argument("output_dir", help="Output directory")
    parser.add_argument("package_name", help="Package name in Ada")

    parser.add_argument("-w", "--wfdb", 
                        action='store_true',
                        help="Read the input file as a wfdb file. " \
                        "File extension must be omitted. ")

    args = parser.parse_args()

    output_file = args.package_name.lower() + ".ads"
    generate_ada_array(args.input, args.output_dir + "/" + output_file, args.wfdb, args.package_name)
