import numpy as np
import sys
import time
"""
This file takes an integer input from the terminal enviroment, and prints all integers starting with 1 and up to the specified integer.
"""
if __name__ == "__main__":
    end_int = int(sys.argv[1])
    for num in range(1, end_int +1):
        print(f"\r{num}", end = "")
        time.sleep(0.2)
    print()
        
