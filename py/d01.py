def readAll():
    out = []
    for line in open("inputs/d01.txt"):
        try:
            out += [int(line)]
        except:
            pass
    return out

lines = readAll()
print(sum(a < b for (a, b) in zip(lines, lines[1:])))

windows = list(map(sum, zip(lines, lines[1:], lines[2:])))
print(sum(a < b for (a, b) in zip(windows, windows[1:])))

