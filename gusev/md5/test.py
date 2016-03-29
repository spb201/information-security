import unittest
import random
import string
from hash import MD5
from hashlib import md5 as hashlib_md5


class TestMD5(unittest.TestCase):
    def test_RFC1321(self):
        rfc1321_test_cases = {
            '': 'd41d8cd98f00b204e9800998ecf8427e',
            'a': '0cc175b9c0f1b6a831c399e269772661',
            'abc': '900150983cd24fb0d6963f7d28e17f72',
            'message digest': 'f96b697d7cb7938d525a2f31aaf161d0',
            'abcdefghijklmnopqrstuvwxyz': 'c3fcd3d76192e4007dfb496cca67e13b',
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789': 'd174ab98d277d9f5a5611c2c9f419d9f',
            '12345678901234567890123456789012345678901234567890123456789012345678901234567890':
                '57edf4a22be3c955ac49da2e2107b67a',
        }

        hash_creator = MD5()
        for msg, _hash in rfc1321_test_cases.items():
            self.assertEqual(hash_creator.md5(msg), _hash)

    def test_compare_with_hashlib(self):
        s = string.ascii_letters
        md5_creator = MD5()

        for i in range(1000):
            hashlib_md_creator = hashlib_md5()
            message = ''.join(random.sample(s, random.randint(0, 50)))
            hashlib_md_creator.update(bytearray(message, 'UTF-8'))
            self.assertEqual(md5_creator.md5(message), hashlib_md_creator.hexdigest())


if __name__ == '__main__':
    unittest.main()
