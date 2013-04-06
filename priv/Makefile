all: capturer_mjpeg

capturer_mjpeg: capturer_mjpeg.c
	gcc -O2 -ljpeg -lgd -o $@ $<
clean:
	rm -f capturer_mjpeg
