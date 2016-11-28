ifndef GHCID_SIZE
  ghcid_size=8
else
  ghcid_size=$(GHCID_SIZE)
endif

stack-ghci=stack ghci

ghci:
	$(stack-ghci)

ghci-test:
	$(stack-ghci) --test

bench:
	stack bench

ghcid:
	ghcid --height=$(ghcid_size) --topmost "--command=$(stack-ghci)"

ghcid-test:
	ghcid --height=$(ghcid_size) --topmost "--command=$(stack-ghci) --test "


