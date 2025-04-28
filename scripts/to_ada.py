#!/usr/bin/env python3
import sys
import os

def generate_ada_array(input_file, output_file, symbol_name="Data"):
    # Read the input binary
    with open(input_file, "rb") as f:
        data = f.read()

    # Open the Ada output file
    with open(output_file, "w") as f:
        basename = os.path.basename(input_file).replace('.', '_')
        
        f.write("with Interfaces;\n")
        f.write(f"package {symbol_name} is\n")
        f.write(f"   Data : constant array (1 .. {len(data)}) of Interfaces.Unsigned_8 := (\n")

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
    if len(sys.argv) < 3:
        print("Usage: python3 generate_ada_array.py <input_binary_file> <output_dir> <Ada_Package_Name>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_dir = sys.argv[2]
    package_name = sys.argv[3]
    output_file = package_name.lower() + ".ads"
    generate_ada_array(input_file, output_dir + "/" + output_file, package_name)
