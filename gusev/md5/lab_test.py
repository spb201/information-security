from hash import MD5
from functools import reduce

hex_bin_map = {'0': '0000', '1': '0001', '2': '0010', '3': '0011',
               '4': '0100', '5': '0101', '6': '0110', '7': '0111',
               '8': '1000', '9': '1001', 'a': '1010', 'b': '1011',
               'c': '1100', 'd': '1101', 'e': '1110', 'f': '1111'}

regimes = (('f', 'g', 'h', 'k'),
           ('g', 'h', 'k', 'f'),
           ('h', 'k', 'f', 'g'),
           ('k', 'f', 'g', 'h'))

strings = ('aaaaa', 'aaaab', 'aaaba', 'aabaa', 'abaaa', 'baaaa')


def hex_to_bin(a):
    return reduce(lambda b, c: b + hex_bin_map[c], a, '')


def hamming_distance(a, b):
    return sum(not q == w for q, w in zip(hex_to_bin(a), hex_to_bin(b)))

for regime in regimes:
    hash_creator = MD5(order=regime)
    row = list(map(hash_creator.md5, strings))

    distances = reduce(lambda acc, i: acc.append(list(hamming_distance(i, j) for j in row)) or acc, row, [])
    distances_wo_zeros = list(map(lambda distance: list(filter(lambda x: not x == 0, distance)), distances))

    mr = sum(map(sum, distances_wo_zeros))/30

    r = sum(sum(map(lambda _d: (mr - _d)**2/30, distance)) for distance in distances_wo_zeros)**0.5

    print(regime)
    for dist in distances:
        print(dist)
    print('mr={0}'.format(mr))
    print('r={0}'.format(r))
    print()
