"""
Couldn't get it to work in haskell for some reason, so made this script to verify the results.
Turned out that I typed [n, n-1, i] instead of [n, n-1 .. i].
"""

loc = open("./testLocations.txt").readlines()
route = open("./testRoute.txt").readlines()

loc = [l.strip().split(": ") for l in loc]


def readTuple(s):
    x = s[1:-1].split(", ")
    return (int(x[0]), int(x[1]))


# done
loc = [(x[0], readTuple(x[1])) for x in loc]

for r in range(len(route)):
    x = route[r].strip()
    # look away please
    coords = loc[[l[0] for l in loc].index(x)][1]
    route[r] = (coords, 0.0)  # coord, time


def norgesTaxiAvstand(x, y):
    xx, xy = x
    yx, yy = y

    return abs(xx - yx) + abs(xy - yy)


def t(santa, pos):
    av = norgesTaxiAvstand(santa, pos)
    if av >= 50:
        return 1.0
    elif av >= 20:
        return 0.75
    elif av >= 5:
        return 0.5
    elif av >= 1:
        return 0.25
    else:
        return 0.0


state = loc

santaPos = [(0, 0)]


def sign(x):
    if x < 0:
        return -1
    else:
        return 1


for r in route:
    xsign = sign(r[0][0] - santaPos[-1][0])

    while santaPos[-1][0] != r[0][0]:
        n = (santaPos[-1][0] + 1*xsign, santaPos[-1][1])
        santaPos.append(n)
    
    ysign = sign(r[0][1] - santaPos[-1][1])

    while santaPos[-1][1] != r[0][1]:
        n = (santaPos[-1][0], santaPos[-1][1] + 1*ysign)
        santaPos.append(n)

print("==== BEGIN CALCULATING TIME ====")
print(f":> Santa positions: {len(santaPos)}")
print(f":> Locations: {len(route)}")


print(santaPos[1:])
# c = 0 
# for p in santaPos[1:]:
#     c += 1

#     if c % 1000 == 0:
#         print(f"   :> Done with {c}")

#     for l in range(len(route)):
#         route[l] = (route[l][0], route[l][1] + t(p, route[l][0]))

# print(route)