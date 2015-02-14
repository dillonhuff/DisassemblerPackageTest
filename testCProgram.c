#include "stdio.h"

int util1(int x, int y) {
  if (x < y) {
    return 32;
  } else {
    return 13;
  }
}

int main() {
  int i;
  for (i = 0; i < 23; i++) {
    printf("A test where i = %d\n", i);
  }
  return 0;
}
