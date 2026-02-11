GHC = ghc
SRC_DIR = src
BIN_DIR = bin
TARGET = $(BIN_DIR)/calculator

SOURCES = $(SRC_DIR)/Calculator.hs $(SRC_DIR)/Parsing.hs

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(SOURCES)
	mkdir -p $(BIN_DIR)
	$(GHC) -i$(SRC_DIR) $(SRC_DIR)/Calculator.hs -o $(TARGET)

clean:
	rm -f $(SRC_DIR)/*.hi $(SRC_DIR)/*.o
	rm -rf $(BIN_DIR)

run: $(TARGET)
	./$(TARGET)
