.PHONY: all
all:
	emacs --batch --eval '(byte-recompile-directory ".")'
