#!/usr/bin/env python

import string

dataf = "./data/tld.dat"
modulef = "./src/Network/URI/TLD/Internal.hs"

with open(dataf) as f:
				tlds = f.readlines()

# Strip newlines.
tlds = map(lambda tld : tld.strip(), tlds)

# Skip empty lines and lines with non-alphanumeric characters.
tlds = filter(lambda tld : tld != "" and set(tld) <= set(string.ascii_lowercase + string.digits + '.'), tlds)

# Write internal module file.
with open(modulef, "w+") as f:
				f.write('{-# LANGUAGE OverloadedStrings #-}\n')
				f.write('\n')
				f.write('module Network.URI.TLD.Internal where\n')
				f.write('\n')
				f.write('import Data.Set (Set)\n')
				f.write('import qualified Data.Set as Set\n')
				f.write('import Data.Text (Text)\n')
				f.write('\n')
				f.write('tldSet :: Set Text\n')
				f.write('tldSet = Set.fromList [\n')
				f.write('      "localhost"\n')
				map(lambda tld : f.write('    , "' + tld + '"\n') , tlds)
				f.write('    ]\n')
				f.write('\n')

# print( tlds)
