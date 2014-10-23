#!/usr/bin/python2

import os
import subprocess

def test(root, codes, wrong, flags=None):
    flags = flags or []
    for root, dirs, files in os.walk(root):
        for file in files:
            if not file.endswith(".wacc"):
                continue
            file = os.path.join(root, file)
            print("\n" + file + ":\n")
            code = subprocess.call(
                ["./dist/build/whacked/whacked", file] + flags)
            if code not in codes:
                wrong[file] = code
                with open(file, 'r') as fin:
                    print fin.read()

    print "\nFailed tests:\n"
    for file, code in wrong.iteritems():
        print file + ': ' + str(code)


def main():
    """Entry point of the script."""
    subprocess.call(["cabal", "build"])
    wrong = {}
    test("../wacc_examples/invalid/syntaxErr", [100], wrong)
    test("../wacc_examples/invalid/semanticErr", [200], wrong)
    test("../wacc_examples/valid", [0], wrong, ["-I"])



if __name__ == "__main__":
    main()