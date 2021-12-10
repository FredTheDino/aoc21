from math import prod

with open("input/d09.txt", "r") as f:
    world = {(x, y) : int(p) for (y, line) in enumerate(f.readlines()) for (x, p) in enumerate(line.strip())}

def neighbors(x, y):
    yield from [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

def isSmallest(world, pos):
    return world.get(pos) < min(world.get(p) or 10 for p in neighbors(*pos))

print(sum(world.get(pos) + 1 for pos in world if isSmallest(world, pos)))

def search(world, seen, pos):
    if any([pos in seen, pos not in world, world.get(pos) == 9]): return 0
    seen.add(pos)
    return 1 + sum(search(world, seen, n) for n in neighbors(*pos))

seen = set()
visited = [search(world, seen, pos) for pos in world]
print(prod(sorted(visited)[-3:]))
