package main

import "fmt"
import "os"
import "math"

func changeEndian(n uint32) uint32 {
    return n & 0xff000000 >> 24 |
        n & 0x00ff0000 >> 8 |
        n & 0x0000ff00 << 8 |
        n & 0x000000ff << 24
}

func hashToHex(digest [4]uint32) string {
    return fmt.Sprintf(
        "%08x%08x%08x%08x",
        changeEndian(digest[0]),
        changeEndian(digest[1]),
        changeEndian(digest[2]),
        changeEndian(digest[3]))
}

func distanceHash(digest1, digest2 [4]uint32) int {
  return hammingUint64(uint64(digest1[0]), uint64(digest2[0])) +
    hammingUint64(uint64(digest1[1]), uint64(digest2[1])) +
    hammingUint64(uint64(digest1[2]), uint64(digest2[2])) +
    hammingUint64(uint64(digest1[3]), uint64(digest2[3]))
}

func printTable(strings [6]string, mode int) {
  sum := 0.0

  for i := 0; i < 6; i++ {
    for j := 0; j < 6; j++ {
      if i == j {
        fmt.Printf("-\t")
      } else {
        distance := distanceHash(hash(strings[i], mode), hash(strings[j], mode))
        sum += float64(distance)
        fmt.Printf("%d\t", distance)
      }
    }
    fmt.Printf("\n")
  }

  mr := sum / 30.0

  rsum := 0.0
  for i := 0; i < 6; i++ {
    for j := 0; j < 6; j++ {
      if i != j {
        rdistance := math.Pow(mr - float64(distanceHash(hash(strings[i], mode), hash(strings[j], mode))), 2) / 30
        rsum += rdistance
      }
    }
  }

  r := math.Sqrt(rsum)

  fmt.Printf("\nmr = %f; r = %f\n\n", mr, r)
}

func main() {
    strings := [6]string{"aaaaa", "aaaab", "aaaba", "aabaa", "abaaa", "baaaa"}
    for i := 0; i < 4; i++ {
      printTable(strings, i)
    }

    //result := hash(os.Args[1], 0)
    //fmt.Println(hashToHex(result))
    //fmt.Println(distanceHash(md5("aaaaa"), md5("aaaaa")))
    //fmt.Println(distanceHash(md5("aaaaa"), md5("aaaab")))
    os.Exit(0)
}