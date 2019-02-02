.PHONY: all
all: indent run

.PHONY: indent
indent:
	hindent BreakOnJust.hs
	hindent Timeout.hs

.PHONY: run
run:
	./BreakOnJust.hs
	./Timeout.hs
