package main

func appendLength(array []byte, length int) []byte {
    bits := length * 8
    return append(
        array,
        byte(bits & 0xff),
        byte((bits >> 8) & 0xff),
        byte((bits >> 16) & 0xff),
        byte((bits >> 24) & 0xff),
        byte((bits >> 32) & 0xff),
        byte((bits >> 40) & 0xff),
        byte((bits >> 48) & 0xff),
        byte((bits >> 56) & 0xff),
    )
}

func breakChunk(chunk []byte) [16]uint32 {
    var words [16]uint32

    for i := 0; i < 16; i++ {
        words[i] = uint32(chunk[i * 4]) |
        uint32(chunk[i * 4 + 1]) << 8 |
        uint32(chunk[i * 4 + 2]) << 16 |
        uint32(chunk[i * 4 + 3]) << 24
    }

    return words
}

func leftRotate(x uint32, amount uint32) uint32 {
    return (x << amount) | (x >> (32 - amount))
}

func hash(message string, mode int) [4]uint32 {
    modes := [4][4]int{}
    modes[0] = [4]int{0, 1, 2, 3}
    modes[1] = [4]int{1, 2, 3, 0}
    modes[2] = [4]int{2, 3, 0, 1}
    modes[3] = [4]int{3, 0, 1, 2}

    fFunctions := make([]func(uint32, uint32, uint32) uint32, 4)
    fFunctions[0] = func(b, c, d uint32) uint32 { return (b & c) | (^b & d) }
    fFunctions[1] = func(b, c, d uint32) uint32 { return (d & b) | (^d & c) }
    fFunctions[2] = func(b, c, d uint32) uint32 { return b ^ c ^ d }
    fFunctions[3] = func(b, c, d uint32) uint32 { return c ^ (b | ^d) }

    gFunctions := make([]func(int) int, 4)
    gFunctions[0] = func(i int) int { return i }
    gFunctions[1] = func(i int) int { return (5 * i + 1) % 16 }
    gFunctions[2] = func(i int) int { return (3 * i + 5) % 16 }
    gFunctions[3] = func(i int) int { return (7 * i) % 16 }

    rotateAmounts := [64]uint32{
        7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
        5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
        4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
        6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,
    }

    // floor(2^32 * abs(sin(i + 1))) for i = 0..63
    constants := [64]uint32{
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
    }

    initValues := [4]uint32{0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476}

    array := []byte(message)
    originalLength := len(array)
    array = append(array, 0x80)

    for len(array) % 64 != 56 {
        array = append(array, 0x00)
    }

    array = appendLength(array, originalLength)

    for j := 0; j < len(array) / 64; j++ {
        chunk := array[j * 64 : (j + 1) * 64]
        words := breakChunk(chunk)

        vals := [4]uint32{
            initValues[0],
            initValues[1],
            initValues[2],
            initValues[3],
        }

        for i := 0; i < 64; i++ {
            var f uint32
            var g int

            if i < 16 {
                f = fFunctions[modes[mode][0]](vals[1], vals[2], vals[3])
                g = gFunctions[0](i)
            } else if i < 32 {
                f = fFunctions[modes[mode][1]](vals[1], vals[2], vals[3])
                g = gFunctions[1](i)
            } else if i < 48 {
                f = fFunctions[modes[mode][2]](vals[1], vals[2], vals[3])
                g = gFunctions[2](i)
            } else {
                f = fFunctions[modes[mode][3]](vals[1], vals[2], vals[3])
                g = gFunctions[3](i)
            }

            d := vals[3]
            vals[3] = vals[2]
            vals[2] = vals[1]
            vals[1] += leftRotate(vals[0] + f + constants[i] + words[g], rotateAmounts[i])
            vals[0] = d
        }

        initValues[0] += vals[0]
        initValues[1] += vals[1]
        initValues[2] += vals[2]
        initValues[3] += vals[3]
    }

    return initValues
}

func md5(message string) [4]uint32 {
    return hash(message, 0)
}

// CBC DES

