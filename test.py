#!/usr/bin/python2

import os
import subprocess


def main():
    """Entry point of the script."""
    subprocess.call(["cabal", "build"])
    for root, dirs, files in os.walk('../wacc_examples/valid'):
        for file in files:
            if not file.endswith(".wacc"):
                continue
            file = os.path.join(root, file)
            print("\n" + file + ":\n")
            subprocess.call(["./dist/build/whacked/whacked", file])


if __name__ == "__main__":
    main()