import copy


def read_to_matrix():
    ret = []
    with open("./input.txt") as f:
        ret = [list(x.strip()) for x in f.readlines()]

    return ret


def get(l, idx):
    (x, y) = idx
    if x == -1 or y == -1:
        raise "whoops"

    try:
        t = l[y][x]

        if t == "#":
            return 1
        elif t == ".":
            return None

        return 0
    except IndexError:
        raise "whoops"

# get number of occupied seats (adjacent)


def get_neighbours(i, matrix):
    (x, y) = i
    vec = [(-1, -1), (0, -1), (1, -1),
           (-1, 0),           (1, 0),
           (-1, 1),  (0, 1),  (1, 1)]

    sum_ = 0

    for velo in vec:
        result = None
        new_pos = (x + velo[0], y + velo[1])
        while result == None:
            try:
                result = get(matrix, new_pos)
                new_pos = (new_pos[0] + velo[0], new_pos[1] + velo[1])
            except:
                result = 0

        sum_ += result

    return sum_
            


def tick(mx):
    mx_copy = copy.deepcopy(mx)

    for y in range(len(mx)):
        for x in range(len(mx_copy[y])):
            if (mx[y][x] == "."):
                continue

            nbs = get_neighbours((x, y), mx)

            if nbs == 0:
                mx_copy[y][x] = "#"
            elif nbs >= 5:
                mx_copy[y][x] = "L"

    return mx_copy


def main():
    mx = read_to_matrix()
    nxmx = tick(mx)

    while mx != nxmx:
        mx = nxmx
        nxmx = tick(mx)

    print("\n".join(["".join(x) for x in nxmx]))

    count = 0
    for r in nxmx:
        for e in r:
            if e == "#":
                count += 1

    print(count)


if __name__ == "__main__":
    main()
