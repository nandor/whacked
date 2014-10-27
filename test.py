#!/usr/bin/python2

import os
import subprocess
import sys

def test(root, codes, wrong, flags=None, compile=False):
    flags = flags or []
    for root, dirs, files in os.walk(root):
        for file in files:
            if not file.endswith(".wacc"):
                continue
            file = os.path.join(root, file)
            print("\n" + file + ":\n")
            sys.stdout.flush()
            sys.stderr.flush()
            code = subprocess.call(
                ["./dist/build/whacked/whacked", file] + flags)
            if code not in codes:
                wrong[file] = code
                with open(file, 'r') as fin:
                    print fin.read()
                continue
            file, _ = os.path.splitext(file)
            subprocess.call(
                ["arm-linux-gnueabi-gcc"
                , "-o" + file
                , "-mcpu=arm1176jzf-s"
                , "-mtune=arm1176jzf-s"
                , file + ".s"])

def main():
    """Entry point of the script."""
    subprocess.call(["cabal", "build"])
    wrong = {}
    #test("../wacc_examples/invalid/syntaxErr", [100], wrong, ["-P"])
    #test("../wacc_examples/invalid/semanticErr", [200], wrong, ["-P"])
    test("../wacc_examples/valid/while", [0], wrong, ["-P"], True)

    print "\nFailed tests:\n"
    fails = []
    for fail, code in wrong.iteritems():
        fails.append((fail, code))
    fails.sort()
    for file, code in fails:
        print file + ': ' + str(code)




if __name__ == "__main__":
    main()