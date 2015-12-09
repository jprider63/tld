tld
===

This project separates subdomains, domains, and top-level-domains from URLs in Haskell. 
At compile-time, it parses a list of top-level-domains from `data/tld.dat`. 
This list comes from `http://mxr.mozilla.org/mozilla/source/netwerk/dns/src/effective_tld_names.dat?raw=1`. 
This package most likely isn't perfect, especially when handling unicode characters. 

You can manually update the TLD list by running the following:

	wget http://mxr.mozilla.org/mozilla/source/netwerk/dns/src/effective_tld_names.dat?raw=1 -O data/tld.dat
	./process.py
	cabal install
