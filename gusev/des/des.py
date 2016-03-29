from functools import reduce

ip1 = [58, 50, 42, 34, 26, 18, 10, 2,
       60, 52, 44, 36, 28, 20, 12, 4,
       62, 54, 46, 38, 30, 22, 14, 6,
       64, 56, 48, 40, 32, 24, 16, 8,
       57, 49, 41, 33, 25, 17,  9, 1,
       59, 51, 43, 35, 27, 19, 11, 3,
       61, 53, 45, 37, 29, 21, 13, 5,
       63, 55, 47, 39, 31, 23, 15, 7]

ip2 = [40, 8, 48, 16, 56, 24, 64, 32,
       39, 7, 47, 15, 55, 23, 63, 31,
       38, 6, 46, 14, 54, 22, 62, 30,
       37, 5, 45, 13, 53, 21, 61, 29,
       36, 4, 44, 12, 52, 20, 60, 28,
       35, 3, 43, 11, 51, 19, 59, 27,
       34, 2, 42, 10, 50, 18, 58, 26,
       33, 1, 41,  9, 49, 17, 57, 25]

pc1 = [56, 48, 40, 32, 24, 16,  8,
        0, 57, 49, 41, 33, 25, 17,
        9,  1, 58, 50, 42, 34, 26,
       18, 10,  2, 59, 51, 43, 35,
       62, 54, 46, 38, 30, 22, 14,
        6, 61, 53, 45, 37, 29, 21,
       13,  5, 60, 52, 44, 36, 28,
       20, 12,  4, 27, 19, 11, 3]

pc2 = [13, 16, 10, 23,  0,  4,
        2, 27, 14,  5, 20,  9,
       22, 18, 11,  3, 25,  7,
       15,  6, 26, 19, 12,  1,
       40, 51, 30, 36, 46, 54,
       29, 39, 50, 44, 32, 47,
       43, 48, 38, 55, 33, 52,
       45, 41, 49, 35, 28, 31]

e = [32,  1,  2,  3,  4,  5,
      4,  5,  6,  7,  8,  9,
      8,  9, 10, 11, 12, 13,
     12, 13, 14, 15, 16, 17,
     16, 17, 18, 19, 20, 21,
     20, 21, 22, 23, 24, 25,
     24, 25, 26, 27, 28, 29,
     28, 29, 30, 31, 32,  1]

s = [[14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7,
      0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8,
      4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0,
      15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13],

     [15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10,
      3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5,
      0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15,
      13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9],

     [10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8,
      13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1,
      13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7,
      1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12],

     [7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15,
      13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9,
      10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4,
      3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14],

     [2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9,
      14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6,
      4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14,
      11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3],

     [12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11,
      10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8,
      9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6,
      4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13],

     [4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1,
      13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6,
      1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2,
      6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12],

     [13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7,
      1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2,
      7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8,
      2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11]]

p = [16,  7, 20, 21, 29, 12, 28, 17,
      1, 15, 23, 26,  5, 18, 31, 10,
      2,  8, 24, 14, 32, 27,  3,  9,
     19, 13, 30,  6, 22, 11,  4, 25]

rotations = [1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1]


def permutate(word, permutation):
    return reduce(lambda res, i: res + word[i - 1], permutation, '')


def word_to_bin(word):
    return reduce(lambda res, ch: res + '{:08b}'.format(ord(ch)), word, '')


def bin_to_word(bword):
    return reduce(lambda r, i: r + chr(int(bword[i:i+8], 2)), range(0, len(bword), 8), '')


def bit_string_xor(a, b):
    assert len(a) == len(b)

    return reduce(lambda res, ch: res + ('0' if ch[0] == ch[1] else '1'), zip(a, b), '')


def sblock(word):
    return reduce(lambda w, i: permutate(w, s[i]), range(8), word)


def feistel(word, key):
    return permutate(sblock(bit_string_xor(permutate(word, e), key)), p)


def rotate(word, n):
    return word[n:len(word)] + word[0:n]


def encode_chunk(word, key):
    assert len(word) == 8

    bword = permutate(word_to_bin(word), ip1)
    l0 = bword[0:32]
    r0 = bword[32:64]

    for ki in generate_keys(key):
        l1 = r0
        r1 = bit_string_xor(l0, feistel(r0, ki))
        l0 = l1
        r0 = r1

    return permutate(l1 + r1, ip2)


def encode_word(word, key):
    return reduce(lambda r, x: r + encode_chunk(word[x:x+8], key), range(0, len(word), 8), '')


def generate_keys(key):
    assert len(key) == 8

    key = word_to_bin(key)

    permuted_key = permutate(key, pc1)
    c = permuted_key[0:28]
    d = permuted_key[28:56]

    for i in range(16):
        c = rotate(c, rotations[i])
        d = rotate(d, rotations[i])
        yield permutate(c + d, pc2)


def cfb_encrypt(word, iv='01234567', key='76543210'):
    assert len(iv) == 8, 'IV should have 8 chars'
    assert len(key) == 8, 'key should have 8 chars'

    while not len(word) % 8 == 0:
        word += ' '
    iv = encode_word(iv, key)

    ct = bit_string_xor(word_to_bin(word[0:8]), iv)
    result = ct

    for i in range(8, len(word), 8):
        ct = bin_to_word(ct)
        r = encode_word(ct, key)
        ct = bit_string_xor(r, word_to_bin(word[i:i+8]))
        result += ct

    return result


def cfb_decrypt(cword, iv='01234567', key='76543210'):
    assert len(cword) % 8 == 0
    assert len(iv) == 8, 'IV should have 8 chars'
    assert len(key) == 8, 'key should have 8 chars'

    iv = encode_word(iv, key)
    ct = cword[0:64]
    plaintext = bin_to_word(bit_string_xor(iv, ct))

    for i in range(64, len(cword), 64):
        ct = bin_to_word(ct)
        r = encode_word(ct, key)
        ct = cword[i:i+64]
        plaintext += bin_to_word(bit_string_xor(ct, r))

    return plaintext


def main():
    iv = '01234567'
    key = '87654321'
    word = 'fsxsddssssxssss!!xx-@'
    print(cfb_decrypt(cfb_encrypt(word)))


if __name__ == '__main__':
    main()
