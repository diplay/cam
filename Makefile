CC=ghc
GHCFLAGS="-O2"
SOURCES=main.hs Cam.hs Lambda.hs Converter.hs
EXECUTABLE=cam

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	$(CC) -o $(EXECUTABLE) -O2 main.hs
