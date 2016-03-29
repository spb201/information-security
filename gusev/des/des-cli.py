import des
import argparse
import sys

parser = argparse.ArgumentParser()
groupMode = parser.add_mutually_exclusive_group(required=True)
groupMode.add_argument('-e', '--encode', dest='encode', action='store_true')
groupMode.add_argument('-d', '--decode', dest='encode', action='store_false')
groupSource = parser.add_mutually_exclusive_group(required=True)
groupSource.add_argument('-w', '--word')
groupSource.add_argument('-f', '--file')
parser.add_argument('-i', '--initial-value', default='01234567')
parser.add_argument('-k', '--key', default='76543210')


args = parser.parse_args()

if args.file:
    with open(args.file, 'r') as f:
        if args.encode:
            print(des.cfb_encrypt(word=f.read().replace('\n', ''), key=args.key, iv=args.initial_value))
        else:
            print(des.cfb_decrypt(cword=f.read().replace('\n', ''), key=args.key, iv=args.initial_value))
    sys.exit(0)

if args.encode:
    print(des.cfb_encrypt(word=args.word, key=args.key, iv=args.initial_value))
else:
    print(des.cfb_decrypt(cword=args.word, key=args.key, iv=args.initial_value))
