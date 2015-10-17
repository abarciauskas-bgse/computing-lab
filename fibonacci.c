#include <stdio.h>

int main(){
  // Something for iteration in our loop
  int i;
  // Initialize the first and second values
  int prev_val;
  int cur_val;
  int new_val;
  prev_val = 1;
  cur_val = 1;

  printf("FIBONACCI SERIES\n");
  printf("01: %i\n", prev_val);
  printf("02: %i\n", cur_val);

  for (i=3;i<=20;i++) {
    // add up the prev and current val
    new_val = prev_val + cur_val;
    // current val should now be that sum
    printf("0%i: %i\n", i, new_val);
    prev_val = cur_val;
    cur_val = new_val;
  }
}
